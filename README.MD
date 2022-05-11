```clojure
(println ((+ 1) 2))

```

->

```ocaml
   Application
       {
         func = Variable "println";
         arg =
           Application
             {
               func = Application { func = Variable "+"; arg = Int 1 };
               arg = Int 2;
             };
       }
```

    ->

```ocaml
    File
      {
        name = "main.go";
        declarations =
          [
            GenDeclaration
              (ImportList { imports = [ { path = "fmt"; name = None } ] });
            FunDeclaration
              {
                name = "main";
                body =
                  [
                    ExprStmt
                      (Application
                         {
                           func = Ident "fmt.PrintLn";
                           arg =
                             BinExpr { op = Add; left = Int 1; right = Int 2 };
                         });
                  ];
              };
          ];
      }
```

    ->

```go
    package main

    import (
      "fmt"
    )

    func main () {
       fmt.Println(1 + 2)
    }

```
