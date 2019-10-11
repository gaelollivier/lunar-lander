open Reprocessing;

let screenWidth = 800.0;
let screenHeight = 600.0;

let backgroundColor = Utils.color(~r=42, ~g=48, ~b=54, ~a=255);

let landerImageRatio = 600.0 /. 437.0;
let landerWidth = 50;
let landerHeight =
  int_of_float(float_of_int(landerWidth) /. landerImageRatio);

let assetsDirectory = "./assets";

let lander = assetsDirectory ++ "/lunar_module.png";

type images = {lander: Reprocessing_Types.Types.imageT};

type assets = {images};

type sceneObject = {
  // Acceleration
  acc: (float, float),
  // Velocity
  vel: (float, float),
  // Position
  pos: (float, float),
};

type scene = {lander: sceneObject};

type state = {
  assets,
  scene,
};

let updateSceneObject =
    (
      {acc: (accX, accY), vel: (velX, velY), pos: (posX, posY)}: sceneObject,
    )
    : sceneObject => {
  acc: (accX, accY),
  vel: (velX +. accX, velY +. accY),
  pos: (posX +. velX, posY +. velY),
};

let setup = env => {
  Env.size(
    ~width=int_of_float(screenWidth),
    ~height=int_of_float(screenHeight),
    env,
  );
  {
    assets: {
      images: {
        lander: Draw.loadImage(~filename=lander, env),
      },
    },
    scene: {
      lander: {
        acc: (0.0, 0.01),
        vel: (1.0, 0.3),
        pos: (50.0, 50.0),
      },
    },
  };
};

let getObjectPos = ((posX, posY)) => (
  int_of_float(posX),
  int_of_float(posY),
);

let draw = (state, env) => {
  let {assets, scene} = state;
  Draw.background(backgroundColor, env);
  Draw.image(
    assets.images.lander,
    ~pos=getObjectPos(scene.lander.pos),
    ~width=landerWidth,
    ~height=landerHeight,
    env,
  );
  {
    assets,
    scene: {
      lander: updateSceneObject(scene.lander),
    },
  };
};

run(~setup, ~draw, ());