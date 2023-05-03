# Marlowe Explorer

Marlowe Explorer is a tool that allows you to explore on-chain Marlowe contracts and watch their execution in Marlowe terms. This document provides instructions on how to build and develop the Marlowe Explorer tool.

## Using Nix

This repo provides a `shell.nix` file that can be used for developing and building Marlowe Explorer without having to worry about dependencies.

### Building with Nix

To build Marlowe Explorer with Nix, you can use the `nix-build` command with `shell.nix`:

```bash
nix-build shell.nix
```

The resulting executable will be made available in `result/bin/marlowe-explorer-exe`

### Development shell

As usual, you can use the `nix-shell` command to enter the development environment from the root folder of the project:

```bash
nix-shell
```

That will make the required Haskell libraries, tools like cabal, stack, the Haskell Language Server, and the `gen-hie` command available in the command line.

### Caching dependencies

In order to take advantage of caching, you can use the `https://nixos.org/channels/nixos-22.11` channel. If using a different channel like `unstable`, building may take a while.

## Using Stack

To build Marlowe Explorer with Stack you just need to write:

```bash
stack build
```

To run Marlowe Explorer with Stack, you can use the following command:

```bash
stack exec marlowe-explorer-exe
```

Both these things can be done from within the development environment, and it will set up Stack for you.

## Using Cabal

To build Marlowe Explorer with Cabal, you can use the following command:

```bash
cabal build
```

To run Marlowe Explorer with Cabal, you can use the following command:

```bash
cabal run
```

Both these things can be done from within the development environment, and it will set up Cabal for you.

## Updating hie.yaml

If you add or move or rename Haskell modules, you will need to update the `hie.yaml` file, which is used by the Haskell Language Server (HLS), you can use the following command from within the Nix shell and the `marlowe-explorer` root folder:

```bash
fix-hie
```

From outside of the development shell, it can be updated using the following command instead:

```bash
gen-hie --stack > hie.yaml
```

Please note that `hie.yaml` files located outside of the project folder (in ancestor folders) may interfere with HLS.

## Executable parameters

Marlowe Explorer provides the following flags that can be used to configure its behavior:

- `--title-label TEXT`: The label to be shown together with the title in the Marlowe Explorer in parenthesis. (It can be used to display the name of the network that the Marlowe explorer is deployed to.) The default value is `Preprod`.

- `--explorer-port PORT`: The port number to use for the Marlowe Explorer server. The default value is `8081`.
    
- `--runtime-host HOSTNAME-OR-IP`: The hostname or IP address of the running Marlowe Runtime server. The default value is `builder`.
    
- `--runtime-port PORT`: The port number of the running Marlowe Runtime server. The default value is `8080`.
    
- `--block-explorer HOST`: The hostname or IP address for exploring Cardano blockchain addresses, transactions, etc. The default value is "preprod.cardanoscan.io".

These flags can be set by using the command line when starting the Marlowe Explorer server. For example, to start the server on port `9000` and connect to a runtime server running `Preview` on the IP address `192.168.0.1` and port `8888`, you could use the following command:

```bash
result/bin/marlowe-explorer-exe --title-label "Preview" --explorer-port 9000 --runtime-host 192.168.0.1 --runtime-port 8888 --block-explorer "preview.cardanoscan.io"
```

If you have built the project using Stack you can pass the parameters by adding `--` between the command and the parameters, like this:

```bash
stack exec marlowe-explorer-exe -- --title-label "Preview" --explorer-port 9000 --runtime-host 192.168.0.1 --runtime-port 8888 --block-explorer "preview.cardanoscan.io"
```


