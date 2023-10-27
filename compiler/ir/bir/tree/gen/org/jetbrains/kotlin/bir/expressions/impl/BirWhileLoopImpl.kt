/*
 * Copyright 2010-2023 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

// This file was generated automatically. See compiler/ir/ir.tree/tree-generator/ReadMe.md.
// DO NOT MODIFY IT MANUALLY.

package org.jetbrains.kotlin.bir.expressions.impl

import org.jetbrains.kotlin.bir.BirElement
import org.jetbrains.kotlin.bir.BirElementVisitorLite
import org.jetbrains.kotlin.bir.SourceSpan
import org.jetbrains.kotlin.bir.acceptLite
import org.jetbrains.kotlin.bir.declarations.BirAttributeContainer
import org.jetbrains.kotlin.bir.expressions.BirExpression
import org.jetbrains.kotlin.bir.expressions.BirWhileLoop
import org.jetbrains.kotlin.bir.types.BirType
import org.jetbrains.kotlin.ir.expressions.IrStatementOrigin

class BirWhileLoopImpl(
    sourceSpan: SourceSpan,
    type: BirType,
    origin: IrStatementOrigin?,
    body: BirExpression?,
    condition: BirExpression,
    label: String?,
) : BirWhileLoop() {
    private var _sourceSpan: SourceSpan = sourceSpan

    override var sourceSpan: SourceSpan
        get() {
            recordPropertyRead()
            return _sourceSpan
        }
        set(value) {
            if (_sourceSpan != value) {
                _sourceSpan = value
                invalidate()
            }
        }

    private var _attributeOwnerId: BirAttributeContainer = this

    override var attributeOwnerId: BirAttributeContainer
        get() {
            recordPropertyRead()
            return _attributeOwnerId
        }
        set(value) {
            if (_attributeOwnerId != value) {
                _attributeOwnerId = value
                invalidate()
            }
        }

    private var _type: BirType = type

    override var type: BirType
        get() {
            recordPropertyRead()
            return _type
        }
        set(value) {
            if (_type != value) {
                _type = value
                invalidate()
            }
        }

    private var _origin: IrStatementOrigin? = origin

    override var origin: IrStatementOrigin?
        get() {
            recordPropertyRead()
            return _origin
        }
        set(value) {
            if (_origin != value) {
                _origin = value
                invalidate()
            }
        }

    private var _body: BirExpression? = body

    override var body: BirExpression?
        get() {
            recordPropertyRead()
            return _body
        }
        set(value) {
            if (_body != value) {
                childReplaced(_body, value)
                _body = value
                invalidate()
            }
        }

    private var _condition: BirExpression? = condition

    override var condition: BirExpression
        get() {
            recordPropertyRead()
            return _condition ?: throwChildElementRemoved("condition")
        }
        set(value) {
            if (_condition != value) {
                childReplaced(_condition, value)
                _condition = value
                invalidate()
            }
        }

    private var _label: String? = label

    override var label: String?
        get() {
            recordPropertyRead()
            return _label
        }
        set(value) {
            if (_label != value) {
                _label = value
                invalidate()
            }
        }
    init {
        initChild(_body)
        initChild(_condition)
    }

    override fun acceptChildrenLite(visitor: BirElementVisitorLite) {
        _body?.acceptLite(visitor)
        _condition?.acceptLite(visitor)
    }

    override fun replaceChildProperty(old: BirElement, new: BirElement?) {
        when {
            this._body === old -> this._body = new as BirExpression?
            this._condition === old -> this._condition = new as BirExpression?
            else -> throwChildForReplacementNotFound(old)
        }
    }
}
