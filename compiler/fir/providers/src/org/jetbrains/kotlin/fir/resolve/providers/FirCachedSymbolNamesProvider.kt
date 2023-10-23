/*
 * Copyright 2010-2023 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.fir.resolve.providers

import org.jetbrains.kotlin.fir.FirSession
import org.jetbrains.kotlin.fir.caches.createCache
import org.jetbrains.kotlin.fir.caches.firCachesFactory
import org.jetbrains.kotlin.fir.caches.getValue
import org.jetbrains.kotlin.fir.resolve.providers.impl.FirSyntheticFunctionInterfaceProviderBase.Companion.mayBeSyntheticFunctionClassName
import org.jetbrains.kotlin.name.ClassId
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.name.Name
import org.jetbrains.kotlin.utils.flatMapToNullableSet

/**
 * A [FirSymbolNamesProvider] that caches all name sets.
 */
abstract class FirCachedSymbolNamesProvider(protected val session: FirSession) : FirSymbolNamesProvider() {
    abstract fun computePackageNames(): Set<String>?

    /**
     * This function is only called if [hasSpecificClassifierPackageNamesComputation] is `true`. Otherwise, the classifier package set will
     * be taken from the cached general package names to avoid building duplicate sets.
     */
    abstract fun computePackageNamesWithTopLevelClassifiers(): Set<String>?

    abstract fun computeTopLevelClassifierNames(packageFqName: FqName): Set<Name>?

    /**
     * This function is only called if [hasSpecificCallablePackageNamesComputation] is `true`. Otherwise, the callable package set will be
     * taken from the cached general package names to avoid building duplicate sets.
     */
    abstract fun computePackageNamesWithTopLevelCallables(): Set<String>?

    abstract fun computeTopLevelCallableNames(packageFqName: FqName): Set<Name>?

    private val cachedPackageNames by lazy(LazyThreadSafetyMode.PUBLICATION) {
        computePackageNames()
    }

    private val topLevelClassifierPackageNames by lazy(LazyThreadSafetyMode.PUBLICATION) {
        if (hasSpecificClassifierPackageNamesComputation) {
            computePackageNamesWithTopLevelClassifiers()?.let { return@lazy it }
        }
        cachedPackageNames
    }

    private val topLevelClassifierNamesByPackage =
        session.firCachesFactory.createCache(::computeTopLevelClassifierNames)

    private val topLevelCallablePackageNames by lazy(LazyThreadSafetyMode.PUBLICATION) {
        if (hasSpecificCallablePackageNamesComputation) {
            computePackageNamesWithTopLevelCallables()?.let { return@lazy it }
        }
        cachedPackageNames
    }

    private val topLevelCallableNamesByPackage =
        session.firCachesFactory.createCache(::computeTopLevelCallableNames)

    override fun getPackageNames(): Set<String>? = cachedPackageNames

    override fun getPackageNamesWithTopLevelClassifiers(): Set<String>? = topLevelClassifierPackageNames

    override fun getTopLevelClassifierNamesInPackage(packageFqName: FqName): Set<Name>? {
        val packageNames = getPackageNamesWithTopLevelClassifiers()
        if (packageNames != null && packageFqName.asString() !in packageNames) return emptySet()

        return getTopLevelClassifierNamesInPackageSkippingPackageCheck(packageFqName)
    }

    // This is used by the compiler `FirCachingCompositeSymbolProvider` to bypass the cache access for classifier package names, because
    // the compiler never computes this package set.
    protected fun getTopLevelClassifierNamesInPackageSkippingPackageCheck(packageFqName: FqName): Set<Name>? =
        topLevelClassifierNamesByPackage.getValue(packageFqName)

    override fun getPackageNamesWithTopLevelCallables(): Set<String>? = topLevelCallablePackageNames

    override fun getTopLevelCallableNamesInPackage(packageFqName: FqName): Set<Name>? {
        val packageNames = getPackageNamesWithTopLevelCallables()
        if (packageNames != null && packageFqName.asString() !in packageNames) return emptySet()

        return topLevelCallableNamesByPackage.getValue(packageFqName)
    }
}

class FirDelegatingCachedSymbolNamesProvider(
    session: FirSession,
    private val delegate: FirSymbolNamesProvider,
) : FirCachedSymbolNamesProvider(session) {
    override fun computePackageNames(): Set<String>? = delegate.getPackageNames()

    override val hasSpecificClassifierPackageNamesComputation: Boolean get() = delegate.hasSpecificClassifierPackageNamesComputation

    override fun computePackageNamesWithTopLevelClassifiers(): Set<String>? =
        delegate.getPackageNamesWithTopLevelClassifiers()

    override fun computeTopLevelClassifierNames(packageFqName: FqName): Set<Name>? =
        delegate.getTopLevelClassifierNamesInPackage(packageFqName)

    override val hasSpecificCallablePackageNamesComputation: Boolean get() = delegate.hasSpecificCallablePackageNamesComputation

    override fun computePackageNamesWithTopLevelCallables(): Set<String>? =
        delegate.getPackageNamesWithTopLevelCallables()

    override fun computeTopLevelCallableNames(packageFqName: FqName): Set<Name>? =
        delegate.getTopLevelCallableNamesInPackage(packageFqName)

    override val mayHaveSyntheticFunctionTypes: Boolean
        get() = delegate.mayHaveSyntheticFunctionTypes

    override fun mayHaveSyntheticFunctionType(classId: ClassId): Boolean = delegate.mayHaveSyntheticFunctionType(classId)
}

open class FirCompositeCachedSymbolNamesProvider(
    session: FirSession,
    val providers: List<FirSymbolNamesProvider>,
) : FirCachedSymbolNamesProvider(session) {
    override fun computePackageNames(): Set<String>? = providers.flatMapToNullableSet { it.getPackageNames() }

    override val hasSpecificClassifierPackageNamesComputation: Boolean = providers.any { it.hasSpecificClassifierPackageNamesComputation }

    override fun computePackageNamesWithTopLevelClassifiers(): Set<String>? =
        providers.flatMapToNullableSet { it.getPackageNamesWithTopLevelClassifiers() }

    override fun computeTopLevelClassifierNames(packageFqName: FqName): Set<Name>? =
        providers.flatMapToNullableSet { it.getTopLevelClassifierNamesInPackage(packageFqName) }

    override val hasSpecificCallablePackageNamesComputation: Boolean = providers.any { it.hasSpecificCallablePackageNamesComputation }

    override fun computePackageNamesWithTopLevelCallables(): Set<String>? =
        providers.flatMapToNullableSet { it.getPackageNamesWithTopLevelCallables() }

    override fun computeTopLevelCallableNames(packageFqName: FqName): Set<Name>? =
        providers.flatMapToNullableSet { it.getTopLevelCallableNamesInPackage(packageFqName) }

    override val mayHaveSyntheticFunctionTypes: Boolean = providers.any { it.mayHaveSyntheticFunctionTypes }

    @OptIn(FirSymbolProviderInternals::class)
    override fun mayHaveSyntheticFunctionType(classId: ClassId): Boolean {
        if (!classId.mayBeSyntheticFunctionClassName()) return false

        // We cannot use `session`'s function type service directly, because the sessions of `providers` aren't necessarily the same as
        // `session`. So we might miss some other session's synthetic function type.
        return providers.any { it.mayHaveSyntheticFunctionType(classId) }
    }

    companion object {
        fun create(session: FirSession, providers: List<FirSymbolNamesProvider>): FirSymbolNamesProvider = when (providers.size) {
            0 -> FirEmptySymbolNamesProvider
            1 -> when (val provider = providers.single()) {
                is FirCachedSymbolNamesProvider -> provider
                else -> FirDelegatingCachedSymbolNamesProvider(session, provider)
            }
            else -> FirCompositeCachedSymbolNamesProvider(session, providers)
        }

        fun fromSymbolProviders(session: FirSession, providers: List<FirSymbolProvider>): FirSymbolNamesProvider =
            create(session, providers.map { it.symbolNamesProvider })
    }
}
