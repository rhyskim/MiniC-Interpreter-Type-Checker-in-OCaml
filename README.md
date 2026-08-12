# MiniC Interpreter & Type Checker in OCaml

## Project Overview
MiniC은 C 언어의 핵심 기능을 추상화한 언어입니다. 이 프로젝트는 MiniC의 인터프리터와 정적 타입 체커를 OCaml로 직접 구현합니다.
어휘 분석(Lexer) - 구문 분석(Parser) - 정적 타입 검사(Type Checker) - 실행(Interpreter)으로 이어지는 컴파일러 파이프라인 전체를 손으로 설계하며, Operational Semantics와 Static Analysis 같은 프로그래밍 언어론의 핵심 개념을 실제 코드로 구현했습니다.

![OCaml](https://img.shields.io/badge/OCaml-EC6813?style=flat-square&logo=ocaml&logoColor=white)
![Dune](https://img.shields.io/badge/Dune-black?style=flat-square)
![Menhir](https://img.shields.io/badge/Menhir-6e7681?style=flat-square)

## Design Highlights
- **Full Pipeline**: `ocamllex`와 `menhir`로 Lexer/Parser를 직접 구현해, 문자열 소스 코드를 AST로 변환하는 전 과정을 손으로 작성했습니다.
- **Static Type Checking**: 프로그램을 실행하기 전에 타입 오류를 잡아내는 정적 타입 체커(`tc.ml`)를 구현해, 실행 시점이 아니라 컴파일 시점에 타입 안정성을 보장합니다.
- **Environment-Store 메모리 모델**: 변수 이름을 주소로 매핑하는 Environment와 주소를 실제 값으로 매핑하는 Store를 분리해(`Mem.ml`, `addrManager.ml`), 포인터 연산과 참조를 명확한 의미론으로 처리했습니다.
- **복잡한 데이터 구조 지원**: 배열, 튜플, 포인터, 중첩 함수 정의를 지원해 실제 C 언어의 동작을 재현했습니다.

## Structure
- `parser/lexer.mll`, `parser/parser.mly`: ocamllex/menhir 기반 어휘·구문 분석기
- `parser/ast.ml`: AST 정의와 Pretty Printer
- `tc.ml`: 정적 타입 체킹 로직
- `interp.ml`: 실행 의미론(Operational Semantics) 구현
- `Mem.ml`, `Value.ml`, `addrManager.ml`, `fstore.ml`: 런타임 값·메모리·주소 관리 모듈
- `GlobalTEnv.ml`, `LocalTEnv.ml`: 타입 체커가 참조하는 전역/지역 타입 환경
- `test.ml`: 인터프리터 동작을 확인하는 테스트 스위트

## Build & Run
필요한 도구: OCaml, [opam](https://opam.ocaml.org/)

```bash
opam install dune menhir bisect_ppx

dune build
dune exec ./test.exe
```
