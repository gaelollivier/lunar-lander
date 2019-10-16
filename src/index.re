open Reprocessing;

let screenWidth = 800.0;
let screenHeight = 600.0;

let viewportRatio = screenWidth /. screenHeight;

let backgroundColor = Utils.color(~r=42, ~g=48, ~b=54, ~a=255);

let landerImageRatio = 600.0 /. 437.0;
let landerWidth = 9.4;
let landerHeight = landerWidth /. landerImageRatio;

let assetsDirectory = "./assets";

let lander = assetsDirectory ++ "/lunar_module.png";

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

let updateSceneObject = (obj: sceneObject): sceneObject => {
  let {acc: (accX, accY), vel: (velX, velY), pos: (posX, posY)} = obj;
  {
    ...obj,
    acc: (accX, accY),
    vel: (velX +. accX, velY +. accY),
    pos: (posX +. velX, posY +. velY),
  };
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
      y: 0.0,
      width: 100.0,
      height: 100.0 /. viewportRatio,
    },
    scene: {
      lander: {
        sprite: Draw.loadImage(~filename=lander, env),
        width: landerWidth,
        height: landerHeight,
        acc: (0.0, (-0.001)),
        vel: (0.05, 0.3),
        pos: (0.0, 0.0),
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

let update = state => {
  ...state,
  scene: {
    lander: updateSceneObject(state.scene.lander),
  },
};

let draw = (state, env) => {
  let {viewport, scene} = state;
  Draw.background(backgroundColor, env);
  Draw.scale(
    ~x=screenWidth /. viewport.width,
    ~y=(-1.0) *. (screenHeight /. viewport.height),
    env,
  );
  Draw.translate(~x=-. viewport.x, ~y=-. viewport.height -. viewport.y, env);
  drawObject(scene.lander, env);
  update(state);
};

run(~setup, ~draw, ());