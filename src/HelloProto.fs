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
    // MESSAGE: RequestAccountMovements
    // -------------------------------------------------------------------------
    type [<RequireQualifiedAccess>] RequestAccountMovements() =
      class
        member val fromAccountId : uint64 = 0UL with get, set
        member val movements : ResizeArray<AccountMovement> = ResizeArray () with get, set
      end
    // -------------------------------------------------------------------------

namespace Hello.BankAccount
  module Accounts =
    // -------------------------------------------------------------------------
    // ENUM: AccountType
    // -------------------------------------------------------------------------
    type AccountType =
      | Deposit = 1
      | Savings = 2
    // -------------------------------------------------------------------------

    // -------------------------------------------------------------------------
    // MESSAGE: Account
    // -------------------------------------------------------------------------
    type [<RequireQualifiedAccess>] Account() =
      class
        member val accountType : AccountType = ``Unrecognized type`` with get, set
        member val accountId : uint64 = 0UL with get, set
        member val accountName : string = "" with get, set
        member val balanceInCents : int64 = 0L with get, set
        member val currency : string = "" with get, set
      end
    // -------------------------------------------------------------------------

    // -------------------------------------------------------------------------
    // MESSAGE: ListAccounts
    // -------------------------------------------------------------------------
    type [<RequireQualifiedAccess>] ListAccounts() =
      class
        member val accounts : ResizeArray<Account> = ResizeArray () with get, set
      end
    // -------------------------------------------------------------------------

// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------

