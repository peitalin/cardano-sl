Overview
========

Software update is a protocol mechanism that allows nodes to
automatically update their executables without changing protocol
itself. Here's the process overview:
1. Update files are uploaded to the dedicated server (S3 storage i
   guess).
2. Software update is proposed. Proposal is a datatype that gets into
   the blockchain. It contains information about version changes and
   hashes of update files.
3. Software update is confirmed by voting from majority of nodes.
4. Nodes that see an update try to download and apply it
   automatically.

Now, again, step-by-step.


Prerequisites
=============
1. Cluster nodes' keys. They are crucial to have since cluster nodes
   are the only nodes that have stake in the system so they are the
   only nodes that can propose an update and vote for it.
2. Access to the server in overview part 1. I'm not sure what is the
   real production value, but it's configured by the flag
   `--update-server URI` of the node executable.
3. Update installers. Those are two executables (exe for win, pkg for
   mac). I'm not sure where do people retrieve them from, but you can
   ask @fersel.

Proposing the update
====================

Ensure your keys are somewhere around and named, say, `nodeX.key`,
where `X ∈ [1..7]`. Procedure is done using `cardano-auxx`
executable. Enter the repl mode (substitute a real peer from cluster):

```
stack exec cardano-auxx -- --system-start 0 --log-config
  scripts/log-templates/log-config-qa.yaml
  --logs-prefix "logs/auxx-update-1.0.1" --db-path auxx-update-1.0.1
  --peer 1.2.3.4:3000 repl
```

Now import secret keys. Command `listaddr` prints currently imported
keys. Command `add-key PATH` will import a key from file. Execute
`add-key nodeX.key` for at least half (1-4) of the cluster nodes'
keys.

Execute propose update command from repl:

```
> propose-update 0 0.0.0 0 20 2000000 csl-daedalus:1 win64 daedalus1.exe
none macos daedalus1c.pkg none
```

First argument is index of imported key you're sending update
from. Get it from `listaddr` output -- any cluster node will
do. Parameters 2-5 are block version data parameters -- leave they as
they are provided in the cli example. `csl-daedalus:N` is software
version update (current N on mainnet is 0 or 1 for different
installers, so we should all update to 1 and then to 2). Then goes the
list of triples -- update installers. Provide the path to installers
without any slashes (installers should be in the same dir as auxx is
launched in).

That's it. Successfull command output looks like this:

```
> propose-update ARGS ...
[smart-wallet:DEBUG] [2017-09-21 15:13:24 MSK] Proposing update...
Read file installer062win64.exe succesfuly, its hash: 01abf1c8b881c2f8ea4d1349a700f29d4088e68dc04b6bf4680ea7e14638373e
Read file installer062macos64.pkg succesfuly, its hash: 3bc1084841fb99fff03ef92bc35eef9a80a20aeca688dfc1b50a4aa6dd6f7c73
[smart-wallet:INFO] [2017-09-21 15:13:25 MSK] Announcing proposal with id 4c827d6fe03d4c3646ebbbc28d4e09c57690e1dcba54b9adc0050d3f76734cf6
Update proposal submitted, upId: 4c827d6fe03d4c3646ebbbc28d4e09c57690e1dcba54b9adc0050d3f76734cf6
```

Crucial info here is proposal ID and installers' hashes.

Uploading update files
======================

Should be easy enough. The only tricky point is that installer file
name should be exactly equal to its hash given you by auxx. Rename,
upload.

Voting for proposal
===================

Launch auxx repl and do for at least half of nodes:
```
> vote N y 4c827d6fe03d4c3646ebbbc28d4e09c57690e1dcba54b9adc0050d3f76734cf6
```

Where `N` should be index of key (from `listadr`), "y" is for yes,
hash is proposal hash. Votes will be sent to the network, software
update will apply soon.
