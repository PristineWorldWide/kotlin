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
    contract { returns(true) implies (this@isHttpError is Result.Failed<Failure.HttpError>) }
    return this is Result.Failed && failure is Failure.HttpError
}

fun <T, F : Failure> test(result: Result<T, F>) {
    if (result.isHttpError()) {
        result.failure.code
    }
}
