open Reprocessing;

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
  // Acceleration
  acc: (float, float),
  // Velocity
  vel: (float, float),
  // Position
  pos: (float, float),
};

type scene = {lander: sceneObject};

type viewport = {
  x: float,
  y: float,
  width: float,
  height: float,
};

type state = {
  viewport,
  scene,
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
  let newAcc = acc;
  let newVel = vel +> acc *> dt;
  let newPos = pos +> vel *> dt;
  // Stop on landing!
  let (newVel, newPos) =
    if (getY(newPos) <= 0.0) {
      (zeroVec, (getX(newPos), 0.0));
    } else {
      (newVel, newPos);
    };
  {...obj, acc: newAcc, vel: newVel, pos: newPos};
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
        acc: gravity,
        vel: zeroVec,
        pos: zeroVec,
      },
    },
  };
};

let drawObject =
    ({sprite, pos: (posX, posY), width, height}: sceneObject, env) => {
  // We draw image in 0,0 of size 1,1 so we can invert it without moving it
  // We need to draw it upside-down because the Y axis is inverted
  let (offsetX, offsetY) = (posX, posY +. height);
  Draw.translate(~x=offsetX, ~y=offsetY, env);
  Draw.scale(~x=width, ~y=-. height, env);
  Draw.image(sprite, ~pos=(0, 0), ~width=1, ~height=1, env);
  // Revert transformations
  Draw.scale(~x=1.0 /. width, ~y=1.0 /. (-. height), env);
  Draw.translate(~x=-. offsetX, ~y=-. offsetY, env);
};

let update = (state, env) => {
  ...state,
  scene: {
    lander: updateSceneObject(state.scene.lander, env),
  },
};

let keyPressed = (state, _env) => {
  let lander = state.scene.lander;
  {
    ...state,
    scene: {
      lander: {
        ...lander,
        vel: (3.0, 1.0),
      },
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

run(~setup, ~draw, ~keyPressed, ());