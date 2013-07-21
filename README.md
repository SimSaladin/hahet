Hahet Configuration Management Tools
====================================

**Disclaimer: This projects is currently in the very beginning of development.
If you wish to contribute, have ideas for the interface, or just simply want to
comment something, you're very welcome to do so!**

Hahet is a yet another configuration management system written in Haskell. It is
intended to be a completely open-source replacement for Puppet/Chef.
The goal is to to provide a conscise, extremely freely extendable configuration
management and integration system.

Hahet leverages Haskell's type system and provides means to:
* Create and apply your own configurations for your systems, based on different
  targets (files, directories, packgaes, ...).
* Write reusable configurations as modules, with your custom interface.
* Easily extend the system by defining your own targets.

Getting started
---------------

In this document a **system** refers to a computer (virtual or not) running some
OS.

A *Configuration* for a system is written as a normal haskell program.  Let's
dive into an example:

```haskell
myConf = do
    use myNginxConf
```

And then compiled to an application which applies the configuration on a system
it is run from.

```haskell
main = do
    app <- confToApp  -- "Compile", check the configuration.
    runHahet app []   -- Optional flags may be given from code OR from the
                      -- command-line.
```

Targets
-------

### Packages

### FileNodes

XXX: Use these combinators instead?

```haskell
/> setOwner "root
/$ setPerms "644"
/< [qc|
```

### Users and Groups

### Mounts

don't add the same disk twice to fstab!

### Package management

### Services (systemd)

```haskell
manage $ Service "sshd"
```

### CronJobs


Modules
-------

**TODO** *How to write a module?*


Implementation details
----------------------

Hahet consists of the *Core* `Hahet.Core`, which defines the interface for
*Modules*. A Module specializes in some component of a system (ssh, webserver,
...) and exposes its own Haskell configuration interface for it.
`Hashet.Modules` provides some default modules.
