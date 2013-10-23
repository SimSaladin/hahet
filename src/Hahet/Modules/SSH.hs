module Hahet.Modules.SSH where

import Hahet
import Hahet.Imports

data SSHClientConf = SSHClientConf

data SSHDaemonConf = SSHDaemonConf

data SSH = SSH { sshPkgs     :: [Pkg]
               , sshConfRoot :: FilePath
               , sshClient   :: SSHClientConf
               , sshDaemon   :: Maybe SSHDaemonConf
               } deriving Typeable

instance Default SSH where
    def = SSH ["openssh"]
              "/etc/ssh"
              undefined
              Nothing

ssh :: C c SSH
ssh = return def

instance PackageManagement c => HahetModule SSH c where

    fromHM mc = do
        manage $ sshPkgs mc
        manage $ directory "/etc/ssh"
                    /- owner "root"
                    /- perms "755"

        manage $ file "/etc/ssh/ssh_config"
                    /- owner "root"
                    /- perms "644"
                    /- fileSource [qc|
Mui.
|]
