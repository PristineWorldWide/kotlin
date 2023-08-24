/*
 * Copyright 2010-2023 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.fir.expressions

import org.jetbrains.kotlin.KtSourceElement
import org.jetbrains.kotlin.fir.FirElementInterface
import org.jetbrains.kotlin.fir.references.FirReference
import org.jetbrains.kotlin.fir.references.FirThisReference
import org.jetbrains.kotlin.fir.types.ConeKotlinType
import org.jetbrains.kotlin.fir.visitors.*

/*
 * This file was generated automatically
 * DO NOT MODIFY IT MANUALLY
 */

abstract class FirInaccessibleReceiverExpression : FirExpression(), FirResolvable {
    abstract override val source: KtSourceElement?
    abstract override val coneTypeOrNull: ConeKotlinType?
    abstract override val annotations: List<FirAnnotation>
    abstract override val calleeReference: FirThisReference

    override fun <R, D> accept(visitor: FirVisitor<R, D>, data: D): R = visitor.visitInaccessibleReceiverExpression(this, data)

    @Suppress("UNCHECKED_CAST")
    override fun <E : FirElementInterface, D> transform(transformer: FirTransformer<D>, data: D): E =
        transformer.transformInaccessibleReceiverExpression(this, data) as E

    abstract override fun replaceConeTypeOrNull(newConeTypeOrNull: ConeKotlinType?)

    abstract override fun replaceAnnotations(newAnnotations: List<FirAnnotation>)

    abstract fun replaceCalleeReference(newCalleeReference: FirThisReference)

    abstract override fun replaceCalleeReference(newCalleeReference: FirReference)

    abstract override fun <D> transformAnnotations(transformer: FirTransformer<D>, data: D): FirInaccessibleReceiverExpression

    abstract override fun <D> transformCalleeReference(transformer: FirTransformer<D>, data: D): FirInaccessibleReceiverExpression
}
