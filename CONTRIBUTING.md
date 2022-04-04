### Installation

1. [Install](https://com-lihaoyi.github.io/mill/#installation) Mill build tool.

2. Clone project:

```dtd
$ cd /Users/{user}/{projects}
$ git clone git://github.com/earogov/ordset
```

3. Run tests (main module):

```dtd
$ cd /Users/{user}/{projects}/ordset
$ mill ordset.test
```

4.a [Optional] Use Visual Studio Code with [Metals](https://scalameta.org/metals/docs/editors/vscode/) plugin.

4.b. [Optional] Configure IntelliJ IDEA project with BSP ([Build Server Protocol](https://build-server-protocol.github.io/)).

- Open IntelliJ IDEA and choose File -> New -> Project from Existing Sources

- Select project directory /Users/{user}/{projects}/ordset

- Choose `Import project from external model` and select `BSP`. New project will be opened with `bsp` toolbar available 
  on the right side.
  
- Configure BSP integration running script:

```dtd
$ cd /Users/{user}/{projects}/ordset
$ mill mill.bsp.BSP/install  
```

- Now you can update IDEA modules definition with button `Reload All BSP Projects` on `bsp` toolbar.
Syntax highlight will be also available in `build.sc` file (Mill build configuration).

Note, project restart may be required for the changes to take effect.

### Release

Put into the project root directory `.env` file with credentials:

```dtd
GPG_PASSPHRASE=*******
GPG_LOCALUSER=*******
SONATYPE_CREDENTIALS=[sonatype_user]:[sonatype_password]
```

Run `release.sh` script. Script publishes artifacts into sonatype staging repository, so further manual actions are
required.