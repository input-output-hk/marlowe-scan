<h2 align="center">
  <a href="" target="blank_">
    <img src="./doc/image/logo.svg" alt="Logo" height="75">
  </a>
  <br>
  MarloweScan (Marlowe Cardano Explorer)
</h2>
  <p align="center">
    <a href="https://github.com/input-output-hk/marlowe-scan/releases"><img src="https://img.shields.io/github/v/release/input-output-hk/marlowe-scan?style=for-the-badge" /></a>
  </p>
<div align="center">
  <a href=""><img src="https://img.shields.io/badge/stability-beta-33bbff.svg" alt="Beta"></a>
  <a href="./LICENSE"><img src="https://img.shields.io/badge/License-Apache_2.0-blue.svg"></a>
  <a href="https://discord.com/invite/cmveaxuzBn"><img src="https://img.shields.io/discord/826816523368005654?label=Chat%20on%20Discord"></a>
</div>

> [!IMPORTANT] 
> This Marlowe repository will soon be moved to https://github.com/marlowe-lang. The new repositories will be administered by an independent vehicle, a not-for-profit organization currently being set up by the transition team.<br> 
> This will allow us to ensure community representation and stewardship. Future developments and support for Marlowe are transitioning to a community-driven model initially led by [Simon Thompson](https://github.com/simonjohnthompson), [Nicolas Henin](https://github.com/nhenin) and [Tomasz Rybarczyk](https://github.com/paluh). <br>
> See [here](https://github.com/marlowe-lang/.github/blob/main/profile/transition.md) for details.

MarloweScan is a tool that allows you to explore on-chain Marlowe contracts and watch their execution in Marlowe terms. This document provides instructions on how to build and develop the MarloweScan tool.

## Marlowe Runtime compatibility

This version of MarloweScan requires a running instance of the Marlowe Runtime. The compatible versions of the Marlowe Runtime include from 0.0.4 up to and including 1.0.0.

## Using Nix

This repo provides a `flake.nix` file that can be used for developing and building MarloweScan without having to worry about dependencies.

### Building with Nix

To build MarloweScan with Nix, you can use the `nix` command:

```bash
nix build
```

The resulting executable will be made available in `result/bin/marlowe-scan-exe`

Alternatively, you can run MarloweScan directly by writing:

```bash
nix run
```

### Development shell

As usual, you can use the `nix` command to enter the development environment from the root folder of the project:

```bash
nix develop
```

That will make the required Haskell libraries, tools like cabal, stack, the Haskell Language Server, and the `fix-hie` command available in the command line.

### Caching dependencies

In order to take advantage of caching, you can use the `https://nixos.org/channels/nixos-23.05` channel.

## Using Stack

To build MarloweScan with Stack you just need to write:

```bash
stack build
```

To run MarloweScan with Stack, you can use the following command:

```bash
stack exec marlowe-scan-exe
```

Both these things can be done from within the development environment, and it will set up Stack for you.

## Using Cabal

To build MarloweScan with Cabal, you can use the following command:

```bash
cabal build
```

To run MarloweScan with Cabal, you can use the following command:

```bash
cabal run
```

Both these things can be done from within the development environment, and it will set up Cabal for you.

## Updating hie.yaml

If you add or move or rename Haskell modules, you will need to update the `hie.yaml` file, which is used by the Haskell Language Server (HLS), you can use the following command from within the Nix shell and the `marlowe-scan` root folder:

```bash
fix-hie
```

From outside of the development shell, it can be updated using the following command instead:

```bash
gen-hie --stack > hie.yaml
```

Please note that `hie.yaml` files located outside of the project folder (in ancestor folders) may interfere with HLS.

## Executable parameters

MarloweScan provides the following flags that can be used to configure its behavior:

- `--title-label TEXT`: The label to be shown together with the title in the MarloweScan in parenthesis. (It can be used to display the name of the network that MarloweScan is deployed to.) The default value is `Preprod`.

- `--marlowe-scan-port PORT`: The port number to use for the MarloweScan server. The default value is `8081`.
    
- `--runtime-host HOSTNAME-OR-IP`: The hostname or IP address of the running Marlowe Runtime server. The default value is `builder`.
    
- `--runtime-port PORT`: The port number of the running Marlowe Runtime server. The default value is `8080`.
    
- `--block-explorer HOST`: The hostname or IP address for exploring Cardano blockchain addresses, transactions, etc. The default value is "preprod.cardanoscan.io".

These flags can be set by using the command line when starting the MarloweScan server. For example, to start the server on port `9000` and connect to a runtime server running `Preview` on the IP address `192.168.0.1` and port `8888`, you could use the following command:

```bash
result/bin/marlowe-scan-exe --title-label "Preview" --marlowe-scan-port 9000 --runtime-host 192.168.0.1 --runtime-port 8888 --block-explorer "preview.cardanoscan.io"
```

If you have built the project using Stack you can pass the parameters by adding `--` between the command and the parameters, like this:

```bash
stack exec marlowe-scan-exe -- --title-label "Preview" --marlowe-scan-port 9000 --runtime-host 192.168.0.1 --runtime-port 8888 --block-explorer "preview.cardanoscan.io"
```

