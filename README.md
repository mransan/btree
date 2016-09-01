# btree
A B-Tree implementation in OCaml

## Overview

This library implement a B-Tree for fixed sized keys and fixed size
values. In other word the size (in bytes) of both the key and the 
value is known in advanced and is constant.

The main OCaml module is [Btree](src/btree.mli).

The B-Tree is assumed to be entirely stored in a single continuous storage. 
That storage can be a file of course but not required. In fact the module 
[Btree_bytes](src/btree_bytes.mli) implement the B-Tree storage in an in-memory byte array. 

The storage is not required to be entirely dedicated to storing the B-Tree; the 
caller application is responsible to allocate new B-Tree node in the 
storage and make sure no other part of the application will override
that data. For instance the caller application could choose to write in the same
file multiple B-Trees.

The core module [Btree](src/btree.mli) does not make any assumption about how the following
disk operation are implemented; they are delegated to the caller 
through the returned type of each module function:
* Reads 
* Writes
* Allocation of new storage

The module [Btree](src/btree.mli) should therefore never be used directly by application code 
but it should rather be used as the core implementation of an opinionated
B-Tree which implements the actual disk operation. In short this library
provides a building block to use B-Trees in your own storage solution. 

Additionally this package provides 2 example of B-Tree implementation which are using 
the [Btree](src/btree.mli) module as their code logic:
* [Btree_bytes](src/btree_bytes.mli): an in-memory byte array is used as storage. This is
   used for unit testing without having to write to a file.
* [Btree_unix](src/btree_unix.mli): uses the `Unix` module part of the OCaml distribution 
   to perform all the file operations.

## Motivation

The original motivation for this project was my [RAFT](https://github.com/mransan/raft-udp) personal project 
which consists in implementing a generic and robust RAFT system. The RAFT protocol requires a reliable file
storage; while I started to look at standard solution like SQLite, I got curious and wanted to learn more 
about file storing and indexing. This library will be a fundamental building block of the storage solution
required for my project. 

Furthermore recent reading picked my interest regarding various topics:
* How to design a library to be independent of disk operation implementation, particularly in the 
  context of asynchronous operation. (see [doc directory](doc/) for more discussion. 
* Not every file system is equal. While all file system implements the posix API they have 
  different behavior (Add missing link)  
* Distributed application in which data is replicated on different node have different storage 
  requirement than application which have a single golden copy of the data. For instance
  if a replica detects that its file system is corrupted, it might simply starts from scratch
  by synchronizing with the other node. A Desktop application has no such a luxury and it might
  need to implement recovery mechanism as part of its storage solution.

Implementing this [Btree](src/btree.mli) was a good way to learn more about the details of 
disk storage (which is far from trivial).  
