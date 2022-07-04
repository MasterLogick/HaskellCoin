# HaskellCoin

Yet another blockchain but written in Haskell this time.

![image](https://user-images.githubusercontent.com/57675148/177044713-0f6aecd5-7988-40b5-8e93-f77ceef7726e.png)

Print `help` to get the list of commands.
In alpha version you can commit transaction from any address to any address without limitations on amount. Just enter `commit` with user hashes and amount and enter `build` to create new block in the network.
Print `show` to list all blocks and transactions in the network.

## Usage
To generate the private key (in case if you do not have one) simply answer `Yes` and the program will do everything by itself. For your convenience, the key was encoded in Base64.
Then, get your `id` by entering: 
```
command> id
```
![image](https://user-images.githubusercontent.com/57675148/177043437-f8752832-340b-421b-bb3c-5113cf6dabf9.png)

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
command> commit RECEIVER1_ID COIN_ANOUNT
command> commit RECEIVER2_ID COIN_ANOUNT
...
command> commit RECEIVERN_ID COIN_ANOUNT
```
![image](https://user-images.githubusercontent.com/57675148/177119311-b0679be3-9502-49e9-a1f1-665dfa674bf3.png)

Then, again build the block:
```
command> build
```
To get all blocks, run:
```
command> show
```
![tg_image_507900195](https://user-images.githubusercontent.com/30959736/176493021-8cedccef-4ff4-4509-9769-ac20691727e2.jpeg)

To get your current keypair:
```
command> key
```
![tg_image_3959235025](https://user-images.githubusercontent.com/30959736/176493200-9122f2ad-9d5e-4427-baae-df28143a0cda.jpeg)

To generate new keypair:
```
command> generate
```
![tg_image_4245662719](https://user-images.githubusercontent.com/30959736/176493395-ecc31af8-1b0f-4b74-8c26-56ea52498c18.jpeg)

To get any user's balance:
```
command> balance ID 
```
![image](https://user-images.githubusercontent.com/57675148/177043655-292c881e-e140-4962-9d7d-09c95901a2b8.png)

At any point, if you get lost, run
```
command> help
```
![tg_image_1856208515](https://user-images.githubusercontent.com/30959736/176494105-6571f6f6-0724-43cc-ae51-05dc5bf57afa.jpeg)

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
