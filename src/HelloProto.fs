// -----------------------------------------------------------------------------
// F# Protobuf wrappers
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// PROTO SPECIFICATION: hello.proto
// -----------------------------------------------------------------------------
namespace Hello.BankAccount
  open ProtobufFs.Wire

  module Movements =
    // -------------------------------------------------------------------------
    // MESSAGE: AccountMovement
    // -------------------------------------------------------------------------
    type AccountMovement() =
      class
        let default_toAccountId : uint64 = 0UL
        let mutable backing_toAccountId = default_toAccountId
        let computeWireSize_toAccountId () = 0UL
        let default_amountInCents : int64 = 0L
        let mutable backing_amountInCents = default_amountInCents
        let computeWireSize_amountInCents () = 0UL
        let default_currency : string = "EUR"
        let mutable backing_currency = default_currency
        let computeWireSize_currency () = 0UL

        member x.ComputeWireSize () =
          0UL
          + computeWireSize_toAccountId ()
          + computeWireSize_amountInCents ()
          + computeWireSize_currency ()
        static member Write  (w : Writer, x : AccountMovement) : unit = ()
        static member Read   (r : Reader, x : byref<AccountMovement>) : bool = false

        member x.toAccountId
          with get () = backing_toAccountId
          and  set v  = backing_toAccountId <- v
        member x.amountInCents
          with get () = backing_amountInCents
          and  set v  = backing_amountInCents <- v
        member x.currency
          with get () = backing_currency
          and  set v  = backing_currency <- v
      end
    // -------------------------------------------------------------------------

    // -------------------------------------------------------------------------
    // MESSAGE: RequestAccountMovements
    // -------------------------------------------------------------------------
    type RequestAccountMovements() =
      class
        let default_fromAccountId : uint64 = 0UL
        let mutable backing_fromAccountId = default_fromAccountId
        let computeWireSize_fromAccountId () = 0UL
        let default_movements : ResizeArray<AccountMovement> = ResizeArray ()
        let mutable backing_movements = default_movements
        let computeWireSize_movements () = 0UL

        member x.ComputeWireSize () =
          0UL
          + computeWireSize_fromAccountId ()
          + computeWireSize_movements ()
        static member Write  (w : Writer, x : RequestAccountMovements) : unit = ()
        static member Read   (r : Reader, x : byref<RequestAccountMovements>) : bool = false

        member x.fromAccountId
          with get () = backing_fromAccountId
          and  set v  = backing_fromAccountId <- v
        member x.movements
          with get () = backing_movements
          and  set v  = backing_movements <- v
      end
    // -------------------------------------------------------------------------

namespace Hello.BankAccount
  open ProtobufFs.Wire

  module Accounts =
    // -------------------------------------------------------------------------
    // ENUM: AccountType
    // -------------------------------------------------------------------------
    type [<RequireQualifiedAccess>] AccountType =
      | Deposit = 1
      | Savings = 2
    // -------------------------------------------------------------------------

    // -------------------------------------------------------------------------
    // MESSAGE: Account
    // -------------------------------------------------------------------------
    type Account() =
      class
        let default_accountType : AccountType = enum 0
        let mutable backing_accountType = default_accountType
        let computeWireSize_accountType () = 0UL
        let default_accountId : uint64 = 0UL
        let mutable backing_accountId = default_accountId
        let computeWireSize_accountId () = 0UL
        let default_accountName : string = ""
        let mutable backing_accountName = default_accountName
        let computeWireSize_accountName () = 0UL
        let default_balanceInCents : int64 = 0L
        let mutable backing_balanceInCents = default_balanceInCents
        let computeWireSize_balanceInCents () = 0UL
        let default_currency : string = "EUR"
        let mutable backing_currency = default_currency
        let computeWireSize_currency () = 0UL

        member x.ComputeWireSize () =
          0UL
          + computeWireSize_accountType ()
          + computeWireSize_accountId ()
          + computeWireSize_accountName ()
          + computeWireSize_balanceInCents ()
          + computeWireSize_currency ()
        static member Write  (w : Writer, x : Account) : unit = ()
        static member Read   (r : Reader, x : byref<Account>) : bool = false

        member x.accountType
          with get () = backing_accountType
          and  set v  = backing_accountType <- v
        member x.accountId
          with get () = backing_accountId
          and  set v  = backing_accountId <- v
        member x.accountName
          with get () = backing_accountName
          and  set v  = backing_accountName <- v
        member x.balanceInCents
          with get () = backing_balanceInCents
          and  set v  = backing_balanceInCents <- v
        member x.currency
          with get () = backing_currency
          and  set v  = backing_currency <- v
      end
    // -------------------------------------------------------------------------

    // -------------------------------------------------------------------------
    // MESSAGE: ListAccounts
    // -------------------------------------------------------------------------
    type ListAccounts() =
      class
        let default_accounts : ResizeArray<Account> = ResizeArray ()
        let mutable backing_accounts = default_accounts
        let computeWireSize_accounts () = 0UL

        member x.ComputeWireSize () =
          0UL
          + computeWireSize_accounts ()
        static member Write  (w : Writer, x : ListAccounts) : unit = ()
        static member Read   (r : Reader, x : byref<ListAccounts>) : bool = false

        member x.accounts
          with get () = backing_accounts
          and  set v  = backing_accounts <- v
      end
    // -------------------------------------------------------------------------

// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------

