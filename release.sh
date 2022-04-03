#!/usr/bin/env bash

set -eux

set -o allexport
source .env
set +o allexport

./mill clean
./mill __.test

./mill mill.scalalib.PublishModule/publishAll \
    __.publishArtifacts \
    "$SONATYPE_CREDENTIALS" \
    --sonatypeUri https://s01.oss.sonatype.org/service/local \
    --sonatypeSnapshotUri https://s01.oss.sonatype.org/content/repositories/snapshots \
    --gpgArgs --local-user=$GPG_LOCALUSER,--passphrase=$GPG_PASSPHRASE,--batch,--yes,-a,-b

./mill versionFile.setNextVersion --bump minor