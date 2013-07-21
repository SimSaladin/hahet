module Hahet.Modules.SSH where

import Hahet

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

    fromModule mc = do
        manage $ sshPkgs mc
        manage $ Directory "/etc/ssh"
                    /- setOwner "root"
                    /- setPerms "755"

        manage $ File "/etc/ssh/ssh_config"
                    /- setOwner "root"
                    /- setPerms "644"
                    >>> [qc|
Mui.
|]
