module Hahet.Modules.SSH where

import Hahet
import Hahet.Imports
default(Text)

data SSHClientConf = SSHClientConf
data SSHDaemonConf = SSHDaemonConf

data SSH = SSH { _sshPkgs     :: [Pkg]
               , _sshConfRoot :: FilePath
               , _sshClient   :: SSHClientConf
               , _sshDaemon   :: Maybe SSHDaemonConf
               } deriving Typeable
makeLenses ''SSH

instance Default SSH where
    def = SSH ["openssh"]
              "/etc/ssh"
              undefined
              Nothing

ssh :: C c SSH
ssh = return def

instance PackageManagement c => HahetModule SSH c where

    fromHM mc = do
        manage $ mc ^. sshPkgs
        manage $ directory "/etc/ssh"
                    /- owner "root"
                    /- perms "755"

        manage $ file "/etc/ssh/ssh_config"
                    /- owner "root"
                    /- perms "644"
                    /- fileSource [qc|
Mui.
|]
