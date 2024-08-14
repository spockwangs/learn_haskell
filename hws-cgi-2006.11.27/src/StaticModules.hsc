-- Copyright 2006, Bjorn Bringert.
module StaticModules (staticModules) where

import ServerState (ModuleDesc)


import Module.Userdir
import Module.Index
import Module.CGI
#ifdef DYNHS
import Module.DynHS
#endif 
import Module.File

staticModules :: [ModuleDesc]
staticModules = 
 [  
  Module.Userdir.desc,
  Module.Index.desc,
  Module.CGI.desc,
#ifdef DYNHS
  Module.DynHS.desc,
#endif
  Module.File.desc
 ]
