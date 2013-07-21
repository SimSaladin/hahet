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

instance PackageManagement c => HahetModule SSH c where
    fromModule ssh = do
        manage $ sshPkgs ssh
        manage $ Directory "/etc/ssh"
        manage $ File "/etc/ssh/ssh_config"
                    /- setOwner "root"
                    >>> ""
