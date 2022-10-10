# Untyped arithmetic expressions


## Opam installation

Full instructions here: [opam installation](https://cs3110.github.io/textbook/chapters/preface/install.html)

```bash
sudo apt install opam
```

```bash
opam init --bare -a -y
```

```bash
eval $(opam env)
```

```bash
opam install -y dune merlin ocaml-lsp-server odoc ocamlformat utop menhir 
```

## Project setup

```bash
dune init proj boolexpr
```

At the end of the file dune-project, add the following line:
```bash
(using menhir 2.1)
```

```bash
dune build
```

```bash
dune utop src
```
