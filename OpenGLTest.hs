module Main where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

-- You can freely have comments here, all comments are stripped by
-- packer..

-- You can indent freely and have type signatures like this one. It is
-- stripped unless the name of that function is mentioned in file
-- opengltest.instructions

main :: IO ()
main = do 
  (progname, _) <- getArgsAndInitialize
  createWindow "A demo in 419 bytes!"
  displayCallback $= display
  mainLoop

display = do
  clear [ColorBuffer]
  renderPrimitive Quads $ do
    color $ (Color3 (1.0::GLfloat) 0 0)
    vertex $ (Vertex3 (0::GLfloat) 0 0)
    vertex $ (Vertex3 (0::GLfloat) 0.2 0)
    vertex $ (Vertex3 (0.2::GLfloat) 0.2 0)
    vertex $ (Vertex3 (0.2::GLfloat) 0 0)
    color $ (Color3 (0::GLfloat) 1 0)
    vertex $ (Vertex3 (0::GLfloat) 0 0)
    vertex $ (Vertex3 (0::GLfloat) (-0.2) 0)
    vertex $ (Vertex3 (0.2::GLfloat) (-0.2) 0)
    vertex $ (Vertex3 (0.2::GLfloat) 0 0)
    color $ (Color3 (0::GLfloat) 0 1)
    vertex $ (Vertex3 (0::GLfloat) 0 0)
    vertex $ (Vertex3 (0::GLfloat) (-0.2) 0)
    vertex $ (Vertex3 ((-0.2)::GLfloat) (-0.2) 0)
    vertex $ (Vertex3 ((-0.2)::GLfloat) 0 0)
    color $ (Color3 (1::GLfloat) 0 1)
    vertex $ (Vertex3 (0::GLfloat) 0 0)
    vertex $ (Vertex3 (0::GLfloat) 0.2 0)
    vertex $ (Vertex3 ((-0.2::GLfloat)) 0.2 0)
    vertex $ (Vertex3 ((-0.2::GLfloat)) 0 0)
  flush
