-- BSD 3-Clause License
-- 
-- Copyright (c) 2018, Kristof Bastiaensen
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
-- 
-- * Redistributions of source code must retain the above copyright notice, this
--   list of conditions and the following disclaimer.
-- 
-- * Redistributions in binary form must reproduce the above copyright notice,
--   this list of conditions and the following disclaimer in the documentation
--   and/or other materials provided with the distribution.
-- 
-- * Neither the name of the copyright holder nor the names of its
--   contributors may be used to endorse or promote products derived from
--   this software without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--
{-# LANGUAGE DefaultSignatures#-}
{-# LANGUAGE ExistentialQuantification #-}

-- | This module is based on the 'aeson-diff-generic' package ideas by Kristof Bastiaensen
-- but SCIM patches are quite different from json patches
module Web.Scim.Class.Patch where
import Data.Text
import Data.Dynamic
import Data.Aeson.Types
import Web.Scim.Schema.Error
import Control.Applicative ((<$>), pure)
import Web.Scim.Schema.PatchOp (Path(..))
import Web.Scim.Filter
import Data.Proxy

-- TODO(arianvp): Add a clear field
data GetSet s = forall v. (Typeable v, FromJSON v, ToJSON v) => GetSet (s -> v) (s -> v -> Either ScimError s)

-- TODO(arianvp):  We need a MultiValue lens as well, that allows us to peek into lists.
class SubAttrLens s where
  subAttrLens :: SubAttr -> Either ScimError (GetSet s)
  removeSubAttr :: SubAttr -> s -> Either ScimError s
  
class PathLens s where
  -- | Map a key to a getter and setter on the given data.
  pathLens :: AttrPath  -> Either ScimError (GetSet s)
  removeAttr :: AttrPath -> s -> Either ScimError s


