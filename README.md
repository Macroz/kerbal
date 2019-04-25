# kerbal

Play [Kerbal Space Program](https://www.kerbalspaceprogram.com/) from Clojure with the help of [KRPC](https://github.com/krpc/krpc).

## Install

- download KRPC jar from https://github.com/krpc/krpc/releases
- install it to local Maven repository a command like this

```sh
mvn install:install-file -Dfile=krpc-java-0.4.8.jar -DgroupId=krpc \
    -DartifactId=krpc-java -Dversion=0.4.8 -Dpackaging=jar
```
- make sure version matches project.clj version
```clj
  :dependencies [...
                 [com.google.protobuf/protobuf-java "3.6.1"]
                 [org.javatuples/javatuples "1.2"]
                 [krpc/krpc-java "0.4.8"]]
```
- download KRPC server zip from 
- extract its `GameData` contents to the `GameData` folder
- start KSP with the KRPC mod installed
- once you launch a game, the server can be started (for more info read KRPC)
- open `core.clj`
- check connection settings i.e. IP and ports match with your server
- launch REPL (in your favourite editor)
- have fun!

## Usage

Read the source!

## License

Do as you will!

Copyright © 2019 Markku Rontu
