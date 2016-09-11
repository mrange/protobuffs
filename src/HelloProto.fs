// -----------------------------------------------------------------------------
// F# Protobuf wrappers
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// PROTO SPECIFICATION: hello.proto
// -----------------------------------------------------------------------------
namespace Hello.BankAccount
  module Movements =
    // -------------------------------------------------------------------------
    // MESSAGE: AccountMovement
    // -------------------------------------------------------------------------
    type [<RequireQualifiedAccess>] AccountMovement() =
      class
        member val toAccountId : uint64 = 0UL with get, set
        member val amountInCents : int64 = 0L with get, set
        member val currency : string = "" with get, set
      end
    // -------------------------------------------------------------------------

    // -------------------------------------------------------------------------
    // MESSAGE: AccountMovements
    // -------------------------------------------------------------------------
    type [<RequireQualifiedAccess>] AccountMovements() =
      class
        member val fromAccountId : uint64 = 0UL with get, set
        member val movements : ResizeArray<AccountMovements> = ResizeArray () with get, set
      end
    // -------------------------------------------------------------------------

namespace Hello.BankAccount
  module Movements2 =
    // -------------------------------------------------------------------------
    // MESSAGE: AccountMovement
    // -------------------------------------------------------------------------
    type [<RequireQualifiedAccess>] AccountMovement() =
      class
        member val toAccountId : uint64 = 0UL with get, set
        member val amountInCents : int64 = 0L with get, set
        member val currency : string = "" with get, set
      end
    // -------------------------------------------------------------------------

    // -------------------------------------------------------------------------
    // MESSAGE: AccountMovements
    // -------------------------------------------------------------------------
    type [<RequireQualifiedAccess>] AccountMovements() =
      class
        member val fromAccountId : uint64 = 0UL with get, set
        member val movements : ResizeArray<AccountMovements> = ResizeArray () with get, set
      end
    // -------------------------------------------------------------------------

// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------

