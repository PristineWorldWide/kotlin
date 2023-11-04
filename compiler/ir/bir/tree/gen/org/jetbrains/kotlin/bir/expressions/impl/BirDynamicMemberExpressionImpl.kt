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
import org.jetbrains.kotlin.bir.expressions.BirDynamicMemberExpression
import org.jetbrains.kotlin.bir.expressions.BirExpression
import org.jetbrains.kotlin.bir.types.BirType

class BirDynamicMemberExpressionImpl(
    sourceSpan: SourceSpan,
    type: BirType,
    memberName: String,
    receiver: BirExpression,
) : BirDynamicMemberExpression() {
    private var _sourceSpan: SourceSpan = sourceSpan

    override var sourceSpan: SourceSpan
        get() {
            recordPropertyRead(5)
            return _sourceSpan
        }
        set(value) {
            if (_sourceSpan != value) {
                _sourceSpan = value
                invalidate(5)
            }
        }

    private var _attributeOwnerId: BirAttributeContainer = this

    override var attributeOwnerId: BirAttributeContainer
        get() {
            recordPropertyRead(2)
            return _attributeOwnerId
        }
        set(value) {
            if (_attributeOwnerId != value) {
                _attributeOwnerId = value
                invalidate(2)
            }
        }

    private var _type: BirType = type

    override var type: BirType
        get() {
            recordPropertyRead(3)
            return _type
        }
        set(value) {
            if (_type != value) {
                _type = value
                invalidate(3)
            }
        }

    private var _memberName: String = memberName

    override var memberName: String
        get() {
            recordPropertyRead(4)
            return _memberName
        }
        set(value) {
            if (_memberName != value) {
                _memberName = value
                invalidate(4)
            }
        }

    private var _receiver: BirExpression? = receiver

    override var receiver: BirExpression
        get() {
            recordPropertyRead(1)
            return _receiver ?: throwChildElementRemoved("receiver")
        }
        set(value) {
            if (_receiver != value) {
                childReplaced(_receiver, value)
                _receiver = value
                invalidate(1)
            }
        }
    init {
        initChild(_receiver)
    }

    override fun acceptChildrenLite(visitor: BirElementVisitorLite) {
        _receiver?.acceptLite(visitor)
    }

    override fun replaceChildProperty(old: BirElement, new: BirElement?) {
        when {
            this._receiver === old -> this._receiver = new as BirExpression?
            else -> throwChildForReplacementNotFound(old)
        }
    }
}
