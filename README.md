# MiniC Interpreter & Type Checker in OCaml

##  Project Overview
본 프로젝트는 OCaml을 활용하여 C언어의 핵심 기능을 추상화한 **MiniC 언어**의 인터프리터 및 정적 타입 체커를 구현한 프로젝트입니다. 
어휘 분석부터 실행까지 컴파일러의 전 과정을 직접 설계하며 프로그래밍 언어론의 핵심 개념을 적용했습니다.


##  Key Features
* [cite_start]**Full Pipeline Implementation**: `ocamllex`와 `menhir`를 이용한 Lexer/Parser 구현[cite: 112].
* [cite_start]**Static Type Checking**: 프로그램 실행 전 타입 안정성을 보장하는 Type System 구축[cite: 46, 51].
* [cite_start]**Advanced Memory Model**: Environment-Store 모델을 기반으로 한 메모리 할당 및 포인터 연산 지원[cite: 59].
* [cite_start]**Rich Data Structures**: `Array`, `Tuple`, `Pointer`, `Nested Functions` 등 복잡한 데이터 타입 처리[cite: 67, 68, 69].

##  Tech Stack
* **Language**: OCaml
* **Tools**: Dune, ocamllex, Menhir
* **Concepts**: Operational Semantics, Static Analysis, Memory Management

##  Structure
* [cite_start]`ast.ml`: Abstract Syntax Tree 정의 및 Pretty Printer [cite: 66]
* [cite_start]`lexer.mll` & `parser.mly`: 문법 정의 및 구문 분석기 생성 [cite: 113, 129]
* [cite_start]`hw14_tc.ml`: 정적 타입 체킹 로직 
* [cite_start]`hw14_interp.ml`: 프로그램 실행 Semantics 구현 [cite: 7]
* [cite_start]`Mem.ml` & `Value.ml`: 런타임 값 및 메모리 관리 모듈 

##  Usage
```bash
# 프로젝트 빌드
dune build

# 테스트 실행
dune exec ./hw14_test.exe
