Hahet Configuration Management
==============================

Hahet is yet another configuration management system which compared to others
aims to leverage the simplicity and strong typing of Haskell to provide a
conscise integration system.

**Currently in the very beginning of development.**

If you wish to contribute, have ideas for the interface, or just simply want to
comment something, you're very welcome to do so!

Implemantation Draft
--------------------

In this document a **system** refers to a computer (virtual or not)
running some OS.

Hahet consists of the *Core* `Hahet.Core`, which defines the interface for
*Modules*. A Module specializes in some component of a system (ssh, webserver,
...) and exposes its own Haskell configuration interface for it.
`Hashet.Modules` provides some default modules.

A *Configuration* for a system is written as a normal haskell program. 
Something along the lines of:

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


