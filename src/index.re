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

let assetsDirectory = "./assets";

let lander = assetsDirectory ++ "/lunar_module.png";

// Moon gravity
let gravity = (0.0, (-1.62));

type sceneObject = {
  sprite: Reprocessing_Types.Types.imageT,
  width: float,
  height: float,
  angle: float,
  // Acceleration
  acc: (float, float),
  // Velocity
  vel: (float, float),
  // Position
  pos: (float, float),
};

type viewport = {
  x: float,
  y: float,
  width: float,
  height: float,
};

type scene = {lander: sceneObject};

type controls = {
  thrust: float,
  roll: float,
};

type state = {
  viewport,
  scene,
  controls,
};

let getX = ((x, _y): (float, float)) => x;
let getY = ((_x, y): (float, float)) => y;
let zeroVec = (0.0, 0.0);

let printVec = ((x, y)) =>
  print_endline(
    "(" ++ string_of_float(x) ++ "," ++ string_of_float(y) ++ ")",
  );

// 2d Addition
let (+>) = ((x1, y1), (x2, y2)) => (x1 +. x2, y1 +. y2);
// 2d Scalar Multiplication
let ( *> ) = ((x1, y1), factor) => (x1 *. factor, y1 *. factor);

let updateSceneObject = (obj: sceneObject, env): sceneObject => {
  let dt = Env.deltaTime(env);
  let {acc, vel, pos} = obj;
  let newVel = vel +> acc *> dt;
  let newPos = pos +> vel *> dt;
  {...obj, vel: newVel, pos: newPos};
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
      width: 100.0,
      height: 100.0 /. viewportRatio,
    },
    scene: {
      lander: {
        sprite: Draw.loadImage(~filename=lander, env),
        width: landerWidth,
        height: landerHeight,
        angle: 0.0,
        acc: zeroVec,
        vel: zeroVec,
        pos: (50.0, 0.0),
      },
    },
    controls: {
      thrust: 0.0,
      roll: 0.0,
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
      lander:
        Env.keyPressed(Space, env)
          ? {
            ...state.scene.lander,
            vel: (10.0, 0.0),
            pos: (10.0, 50.0),
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
          0.01;
        } else if (Env.keyPressed(Right, env)) {
          (-0.01);
        } else if (Env.keyReleased(Left, env) || Env.keyReleased(Right, env)) {
          0.0;
        } else {
          state.controls.roll;
        },
    },
  };
};

let updateLander = (state, _env) => {
  // Stop on landing!
  let {vel, pos, angle} = state.scene.lander;
  let thrustAngle = angle +. pi /. 2.0;
  let thrustVec =
    (cos(thrustAngle), sin(thrustAngle)) *> (state.controls.thrust *. 5.0);
  let landed = getY(pos) < 0.0;
  {
    ...state,
    scene: {
      lander: {
        ...state.scene.lander,
        acc: gravity +> thrustVec,
        vel: landed ? zeroVec : vel,
        pos: landed ? (getX(pos), 0.0) : pos,
        angle: landed ? 0.0 : angle +. state.controls.roll,
      },
    },
  };
};

let update = (state, env) => {
  let state = handleEvents(state, env);
  let state = updateLander(state, env);
  {
    ...state,
    scene: {
      lander: updateSceneObject(state.scene.lander, env),
    },
  };
};

let draw = (state, env) => {
  let {viewport, scene} = state;
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
  drawObject(scene.lander, env);
  update(state, env);
};

run(~setup, ~draw, ());