#!/usr/bin/env bash

set -ex 

GO111MODULE=on go install golang.org/x/tools/gopls@latest
go install github.com/rogpeppe/godef@latest
go install github.com/josharian/impl@latest
go install github.com/haya14busa/gopkgs/cmd/gopkgs@latest
go install github.com/godoctor/godoctor@latest
go install github.com/fatih/gomodifytags@latest
go install github.com/davidrjenni/reftools/cmd/fillstruct@latest
go install github.com/cweill/gotests/...@latest
go install golang.org/x/tools/cmd/guru@latest
go install golang.org/x/tools/cmd/gorename@latest
go install golang.org/x/tools/cmd/goimports@latest
go install golang.org/x/tools/cmd/godoc@latest
GO111MODULE=on CGO_ENABLED=0 go install -v -trimpath -ldflags '-s -w' github.com/golangci/golangci-lint/cmd/golangci-lint@latest
go install github.com/mdempsky/gocode@latest 
go install github.com/zmb3/gogetdoc@latest
go install github.com/go-delve/delve/cmd/dlv@latest
