// !OPT_IN: kotlin.contracts.ExperimentalContracts
// ISSUE: KT-45683

import kotlin.contracts.*

sealed class Failure {
    class Exception(val message: String) : Failure()
    class HttpError(val code: Int) : Failure()
}

sealed class Result<out T, out F : Failure> {
    class Success<T>(val data: T) : Result<T, Nothing>()
    class Failed<F : Failure>(val failure: F) : Result<Nothing, F>()
}

fun <T, F : Failure> Result<T, F>.isHttpError(): Boolean {
    <!ERROR_IN_CONTRACT_DESCRIPTION!>contract<!> { returns(true) implies (this@isHttpError is <!CANNOT_CHECK_FOR_ERASED!>Result.Failed<Failure.HttpError><!>) }
    return this is Result.Failed && <!DEBUG_INFO_IMPLICIT_RECEIVER_SMARTCAST!>failure<!> is Failure.HttpError
}

fun <T, F : Failure> test(result: Result<T, F>) {
    if (result.isHttpError()) {
        result.<!UNRESOLVED_REFERENCE!>failure<!>.<!DEBUG_INFO_MISSING_UNRESOLVED!>code<!>
    }
}
