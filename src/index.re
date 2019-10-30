open Reprocessing;

let pi = 3.14159265;

let screenWidth = 800.0;
let screenHeight = 600.0;

let viewportRatio = screenWidth /. screenHeight;

let backgroundColor = Utils.color(~r=42, ~g=48, ~b=54, ~a=255);
let groundColor = Utils.color(~r=137, ~g=137, ~b=137, ~a=255);

let landerImageRatio = 600.0 /. 437.0;
let landerWidth = 9.4;
let landerHeight = landerWidth /. landerImageRatio;

// Moon gravity
let moonGravity = 1.622;

// Sources:
// - https://nssdc.gsfc.nasa.gov/nmc/spacecraft/display.action?id=1969-059C
// - https://en.wikipedia.org/wiki/Apollo_Lunar_Module
// Masses in Kg
let totalPropellantMass = 8248.0;
// Total LM mass - propellant mass
let landerDryMass = 15103.0 -. totalPropellantMass;
// Max thrust in N
let maxThrust = 45000.0;
let isp = 311.0;
let totalImpulse = isp *. totalPropellantMass *. moonGravity;
let totalBurnTime = totalImpulse /. maxThrust;

let veq = isp *. moonGravity;
let massFlowRate = maxThrust /. veq;

// Totally eye-balling the RCS position relative to center of mass...
let rcsPosition = (2.1, 1.7);
let rcsPositionLength = Vec.length(rcsPosition);
// Somehow the RCS thrust seems to be really low... So I boost it *5 to make it
// playable
// https://en.wikipedia.org/wiki/R-4D
let rcsThrust = 490.0 *. 8.0;

let assetsDirectory = "./assets";

let landerSprite = assetsDirectory ++ "/lunar_module.png";
let digitsFont = assetsDirectory ++ "/font-digits_2x.fnt";
let hudBackground = assetsDirectory ++ "/HUD.png";

type sceneObject = {
  sprite: Reprocessing_Types.Types.imageT,
  width: float,
  height: float,
  // Acceleration
  acc: (float, float),
  // Velocity
  vel: (float, float),
  // Position
  pos: (float, float),
  angularAcc: float,
  angularVel: float,
  angle: float,
};

type viewport = {
  x: float,
  y: float,
  width: float,
  height: float,
};

type hud = {background: Reprocessing_Types.Types.imageT};

type scene = {
  lander: sceneObject,
  hud,
};

type fonts = {digits: fontT};

type controls = {
  thrust: float,
  roll: float,
};

type lander = {propellantMass: float};

type state = {
  viewport,
  scene,
  fonts,
  controls,
  lander,
};

let setup = env => {
  Env.size(
    ~width=int_of_float(screenWidth),
    ~height=int_of_float(screenHeight),
    env,
  );
  {
    viewport: {
      x: 0.0,
      y: (-10.0),
      width: 200.0,
      height: 200.0 /. viewportRatio,
    },
    scene: {
      hud: {
        background: Draw.loadImage(~filename=hudBackground, env),
      },
      lander: {
        sprite: Draw.loadImage(~filename=landerSprite, env),
        width: landerWidth,
        height: landerHeight,
        acc: Vec.zero,
        vel: Vec.zero,
        pos: (50.0, 0.0),
        angularAcc: 0.0,
        angularVel: 0.0,
        angle: 0.0,
      },
    },
    fonts: {
      digits: Draw.loadFont(~filename=digitsFont, ~isPixel=true, env),
    },
    controls: {
      thrust: 0.0,
      roll: 0.0,
    },
    lander: {
      propellantMass: totalPropellantMass,
    },
  };
};

let drawObject =
    ({sprite, pos: (posX, posY), angle, width, height}: sceneObject, env) => {
  Draw.pushMatrix(env);
  // We draw image in -0.5,-0.5 of size 1,1 so we can rotate and invert it
  // We need to draw it upside-down because the Y axis is inverted
  let (offsetX, offsetY) = (posX, posY +. height /. 2.0);
  Draw.translate(~x=offsetX, ~y=offsetY, env);
  Draw.rotate(angle, env);
  Draw.scale(~x=width, ~y=-. height, env);
  Draw.imagef(sprite, ~pos=((-0.5), (-0.5)), ~width=1.0, ~height=1.0, env);

  Draw.popMatrix(env);
};

let handleEvents = (state, env) => {
  {
    ...state,
    scene: {
      ...state.scene,
      lander:
        Env.keyPressed(Space, env)
          ? {
            ...state.scene.lander,
            vel: (25.0, 0.0),
            pos: (10.0, 100.0),
            angularAcc: 0.0,
            angularVel: 0.0,
            angle: pi /. 3.0,
          }
          : state.scene.lander,
    },
    controls: {
      thrust:
        if (Env.keyPressed(Up, env)) {
          1.0;
        } else if (Env.keyReleased(Up, env)) {
          0.0;
        } else {
          state.controls.thrust;
        },
      roll:
        if (Env.keyPressed(Left, env)) {
          1.0;
        } else if (Env.keyPressed(Right, env)) {
          (-1.0);
        } else if (Env.keyReleased(Left, env) || Env.keyReleased(Right, env)) {
          0.0;
        } else {
          state.controls.roll;
        },
    },
  };
};

let updateLander = (state, env) => {
  let dt = Env.deltaTime(env);
  let {propellantMass} = state.lander;
  let {vel, pos, angularVel, angle} = state.scene.lander;
  let gravityAcceleration = (0.0, -. moonGravity);

  let thrust = propellantMass > 0.0 ? maxThrust *. state.controls.thrust : 0.0;
  let thrustAngle = angle +. pi /. 2.0;
  let thrustVec = (cos(thrustAngle), sin(thrustAngle));
  let currentMass = landerDryMass +. propellantMass;
  let thrustAcceleration = Vec.(thrustVec *> (thrust /. currentMass));

  // Calculate expelled propellant
  let massFlowRate = thrust /. veq;
  let newPropellantMass = propellantMass -. massFlowRate *. dt;

  // Consider rcs thrust perfectly perpendicular to their axis with CoM
  // Torque ends-up being just r * F
  // https://en.wikipedia.org/wiki/Torque
  let torque = rcsPositionLength *. rcsThrust *. state.controls.roll;

  // Approximate moment of inertia by treating the lander as a sphere of radius
  // of the lander width...
  let landerRadius = landerWidth /. 2.0;
  let momentOfInertia =
    2.0 /. 5.0 *. (currentMass *. landerRadius *. landerRadius);
  let angularAcc = torque /. momentOfInertia;

  // Stop on landing!
  let landed = Vec.getY(pos) < 0.0;
  {
    ...state,
    scene: {
      ...state.scene,
      lander: {
        ...state.scene.lander,
        acc: Vec.(gravityAcceleration +> thrustAcceleration),
        vel: landed ? Vec.zero : vel,
        pos: landed ? (Vec.getX(pos), 0.0) : pos,
        angularAcc: landed ? 0.0 : angularAcc,
        angularVel: landed ? 0.0 : angularVel,
      },
    },
    lander: {
      propellantMass: newPropellantMass,
    },
  };
};

let updateSceneObject = (obj: sceneObject, env): sceneObject => {
  let dt = Env.deltaTime(env);
  let {acc, vel, pos, angularAcc, angularVel, angle} = obj;
  {
    ...obj,
    vel: Vec.(vel +> acc *> dt),
    pos: Vec.(pos +> vel *> dt),
    angularVel: angularVel +. angularAcc *. dt,
    angle: angle +. angularVel *. dt,
  };
};

let update = (state, env) => {
  let state = handleEvents(state, env);
  let state = updateLander(state, env);
  {
    ...state,
    scene: {
      ...state.scene,
      lander: updateSceneObject(state.scene.lander, env),
    },
  };
};

let drawNumber = (state, number, (posX, posY), env) => {
  let textWidth =
    Draw.textWidth(
      ~body=Printf.sprintf("%.2f", number),
      ~font=state.fonts.digits,
      env,
    );
  Draw.text(
    ~body=Printf.sprintf("%.2f", number),
    ~font=state.fonts.digits,
    // Position is relative to top right, so text is right-aligned
    ~pos=(posX - textWidth, posY),
    env,
  );
};

let drawHUD = (state, env) => {
  Draw.image(
    state.scene.hud.background,
    ~pos=(int_of_float(screenWidth) - 190, 10),
    env,
  );
  let (horizontalVelocity, verticalVelocity) = state.scene.lander.vel;
  let rightPos = 39;
  drawNumber(
    state,
    verticalVelocity,
    (int_of_float(screenWidth) - rightPos, 100),
    env,
  );
  drawNumber(
    state,
    horizontalVelocity,
    (int_of_float(screenWidth) - rightPos, 190),
    env,
  );
};

let draw = (state, env) => {
  let {viewport, scene} = state;
  Draw.pushMatrix(env);
  Draw.background(backgroundColor, env);
  // World to screen transform
  Draw.scale(
    ~x=screenWidth /. viewport.width,
    ~y=(-1.0) *. (screenHeight /. viewport.height),
    env,
  );
  Draw.translate(~x=-. viewport.x, ~y=-. viewport.height -. viewport.y, env);
  // Ground
  Draw.fill(groundColor, env);
  Draw.rect(~pos=((-1000), (-1000)), ~width=2000, ~height=1000, env);
  // Lander
  drawObject(scene.lander, env);
  Draw.popMatrix(env);
  // Draw HUD
  drawHUD(state, env);
  update(state, env);
};

run(~setup, ~draw, ());