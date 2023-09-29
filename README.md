<p align="center">
  <a href="https://github.com/CardanoSolutions/cardano-node/actions/workflows/build.yml"><img src="https://img.shields.io/github/actions/workflow/status/cardanosolutions/cardano-node/build.yml?style=for-the-badge"/></a>
</p>

---

# Overview

This repository is a fork of [input-output-hk/cardano-node](https://github.com/input-output-hk/cardano-node) which aims at experimenting new features which may or may not end up in the original node implementation. Our goal is to **play with new interfaces**, and harness unexploited possibilities of the Cardano node **without causing disruption** on the core team day-to-day maintenance work.

There's therefore no intention whatsoever to change anything regarding the semantics of the existing consensus protocol or to alter any of the ledger rules.

## Additions

### Ledger Events

We introduce a new (optional) command-line option `--ledger-event-handler TCP/PORT` to the `run` command of the node. If provided, the cardano-node will open a TCP socket on the given port and await for a client connection. From there, any event emitted by the ledger will be pushed through that socket as a serialized CBOR object, anchored in a block header hash and a slot. Those [ledger events](https://github.com/input-output-hk/cardano-ledger/blob/master/docs/LedgerEvents.md) are currently produced by the cardano-node but discarded by the consensus layer in the original implementation.

Anchored events abides by the following CDDL schema:

```cddl
rule =
  [ version                    ; The codec version used to encode the following event.
  , bytes .cbor anchored-event ; An CBOR-encoded anchored ledger event.
  ]

anchored-event =
  { 0: block-header-hash ; The block header hash from where this event was emitted.
  , 1: slot              ; The slot number corresponding to the aforementioned header hash.
  , 2: ledger-event      ; The actual ledger event.
  }
```

The complete schema for `ledger-event`, as well as additional documentation for the nature of those events is given in [cardano-node/ledger-events.cddl](https://github.com/CardanoSolutions/cardano-node/blob/release/8.1.2/cardano-node/ledger_events.cddl).
