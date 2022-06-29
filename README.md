# HaskellCoin

Yet another blockchain but written in Haskell this time.

![2022-06-29 15 52 34](https://user-images.githubusercontent.com/87667725/176442084-0439de5f-89ab-4b16-b3ac-e69c48b6ffb9.jpg)

Print `help` to get the list of commands.
In alpha version you can commit transaction from any address to any address without limitations on amount. Just enter `commit` with user hashes and amount and enter `build` to create new block in the network.
Print `show` to list all blocks and transactions in the network.

## Usage
To generate the private key (in case if you do not have one) simply answer `Yes` and the program will do everything by itself. For your convenience, the key was encoded in Base64.
Then, get your `id` by entering: 
```
command> id
```
If you have saved the private key from your previous session, and at least one machine was up all time you were offline or you saved the blockchain by entering
```
command> writeFile /path/to/file
```
If you want to load the blockchain from file, run:
```
command> loadFile /path/to/file
```
Then you share your id with your friends. At first, you will not have any coins binded with your account, to get some, run 
```
command> build
```
This way, you will generate an empty block which will propagate over the net.
Speaking of the Net, if you managed to find somebody brave enough to run this project, connect to them by
```
command> connect FRIEND's_IP PORT
```
Then, to commit transaction, enter:
```
command> commit YOUR_ID RECEIVER1_ID COIN_ANOUNT
command> commit YOUR_ID RECEIVER2_ID COIN_ANOUNT
...
command> commit YOUR_ID RECEIVERN_ID COIN_ANOUNT
```
Then, again build the block:
```
command> build
```
To get all blocks, run:
```
command> show
```
To get your current keypair:
```
command> key
```
To generate new keypair:
```
command> generate
```
To get any user's balance:
```
command> balance ID 
```
At any point, if you get lost, run
```
command> help
```
This will print help message.

## Development

The project is developed with both stack and cabal.

### Building from sources

For building with `cabal`:
```sh
git clone https://github.com/MasterLogick/HaskellCoin
cd HaskellCoin
cabal v2-build
```
And for starting HaskellCoin just run `cabal v2-run` 

For building using `stack`:
```sh
git clone https://github.com/MasterLogick/HaskellCoin
cd HaskellCoin
stack build
```
And for starting HaskellCoin just run `stack run`

## Alpha version description:

Minimal blockchain system. The whole chain runs on the local machine without any network rules or transaction signing. CLI only.

Features:
+ Commit new trasnactino to pending block
+ Build new block from pending transactions
+ Show all blocks and transactions
+ Show current balance of specified user


## Release version features:

+ Blockchain explorer
+ Migrating to PoW (Proof of Work)
+ Saving blockchain to hard drive
+ Digital signature for every user (ECDSA on NIST-P521R1)
+ Propagating blocks over the Internet
+ Network rules
