/*
 * Copyright 2010-2023 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.test.runners.codegen;

import com.intellij.testFramework.TestDataPath;
import org.jetbrains.kotlin.test.util.KtTestUtil;
import org.jetbrains.kotlin.test.TargetBackend;
import org.jetbrains.kotlin.test.TestMetadata;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.util.regex.Pattern;

/** This class is generated by {@link org.jetbrains.kotlin.test.generators.GenerateCompilerTestsKt}. DO NOT MODIFY MANUALLY */
@SuppressWarnings("all")
public class FirPsiSpecificBlackBoxCodegenTestGenerated extends AbstractFirPsiBlackBoxCodegenTest {
    @Nested
    @TestMetadata("compiler/fir/fir2ir/testData/codegen/box")
    @TestDataPath("$PROJECT_ROOT")
    public class Box {
        @Test
        public void testAllFilesPresentInBox() throws Exception {
            KtTestUtil.assertAllTestsPresentByMetadataWithExcluded(this.getClass(), new File("compiler/fir/fir2ir/testData/codegen/box"), Pattern.compile("^(.+)\\.kt$"), null, TargetBackend.JVM_IR, true);
        }

        @Test
        @TestMetadata("sample.kt")
        public void testSample() throws Exception {
            runTest("compiler/fir/fir2ir/testData/codegen/box/sample.kt");
        }

        @Nested
        @TestMetadata("compiler/fir/fir2ir/testData/codegen/box/properties")
        @TestDataPath("$PROJECT_ROOT")
        public class Properties {
            @Test
            public void testAllFilesPresentInProperties() throws Exception {
                KtTestUtil.assertAllTestsPresentByMetadataWithExcluded(this.getClass(), new File("compiler/fir/fir2ir/testData/codegen/box/properties"), Pattern.compile("^(.+)\\.kt$"), null, TargetBackend.JVM_IR, true);
            }

            @Nested
            @TestMetadata("compiler/fir/fir2ir/testData/codegen/box/properties/backingField")
            @TestDataPath("$PROJECT_ROOT")
            public class BackingField {
                @Test
                public void testAllFilesPresentInBackingField() throws Exception {
                    KtTestUtil.assertAllTestsPresentByMetadataWithExcluded(this.getClass(), new File("compiler/fir/fir2ir/testData/codegen/box/properties/backingField"), Pattern.compile("^(.+)\\.kt$"), null, TargetBackend.JVM_IR, true);
                }

                @Test
                @TestMetadata("backingFieldVisibility.kt")
                public void testBackingFieldVisibility() throws Exception {
                    runTest("compiler/fir/fir2ir/testData/codegen/box/properties/backingField/backingFieldVisibility.kt");
                }

                @Test
                @TestMetadata("charSequenceWithBackingField1.kt")
                public void testCharSequenceWithBackingField1() throws Exception {
                    runTest("compiler/fir/fir2ir/testData/codegen/box/properties/backingField/charSequenceWithBackingField1.kt");
                }

                @Test
                @TestMetadata("charSequenceWithBackingField2.kt")
                public void testCharSequenceWithBackingField2() throws Exception {
                    runTest("compiler/fir/fir2ir/testData/codegen/box/properties/backingField/charSequenceWithBackingField2.kt");
                }

                @Test
                @TestMetadata("charSequenceWithBackingField3.kt")
                public void testCharSequenceWithBackingField3() throws Exception {
                    runTest("compiler/fir/fir2ir/testData/codegen/box/properties/backingField/charSequenceWithBackingField3.kt");
                }

                @Test
                @TestMetadata("charSequenceWithBackingField4.kt")
                public void testCharSequenceWithBackingField4() throws Exception {
                    runTest("compiler/fir/fir2ir/testData/codegen/box/properties/backingField/charSequenceWithBackingField4.kt");
                }

                @Test
                @TestMetadata("charSequenceWithBackingField5.kt")
                public void testCharSequenceWithBackingField5() throws Exception {
                    runTest("compiler/fir/fir2ir/testData/codegen/box/properties/backingField/charSequenceWithBackingField5.kt");
                }

                @Test
                @TestMetadata("explicitBackingFieldInAnonymous.kt")
                public void testExplicitBackingFieldInAnonymous() throws Exception {
                    runTest("compiler/fir/fir2ir/testData/codegen/box/properties/backingField/explicitBackingFieldInAnonymous.kt");
                }

                @Test
                @TestMetadata("getterReturnTypeWithBackingField.kt")
                public void testGetterReturnTypeWithBackingField() throws Exception {
                    runTest("compiler/fir/fir2ir/testData/codegen/box/properties/backingField/getterReturnTypeWithBackingField.kt");
                }

                @Test
                @TestMetadata("independentBackingFieldType.kt")
                public void testIndependentBackingFieldType() throws Exception {
                    runTest("compiler/fir/fir2ir/testData/codegen/box/properties/backingField/independentBackingFieldType.kt");
                }

                @Test
                @TestMetadata("lateinitBackingFields.kt")
                public void testLateinitBackingFields() throws Exception {
                    runTest("compiler/fir/fir2ir/testData/codegen/box/properties/backingField/lateinitBackingFields.kt");
                }

                @Test
                @TestMetadata("overriddenPropertiesWithExplicitBackingFields.kt")
                public void testOverriddenPropertiesWithExplicitBackingFields() throws Exception {
                    runTest("compiler/fir/fir2ir/testData/codegen/box/properties/backingField/overriddenPropertiesWithExplicitBackingFields.kt");
                }
            }

            @Nested
            @TestMetadata("compiler/fir/fir2ir/testData/codegen/box/properties/synthetic")
            @TestDataPath("$PROJECT_ROOT")
            public class Synthetic {
                @Test
                public void testAllFilesPresentInSynthetic() throws Exception {
                    KtTestUtil.assertAllTestsPresentByMetadataWithExcluded(this.getClass(), new File("compiler/fir/fir2ir/testData/codegen/box/properties/synthetic"), Pattern.compile("^(.+)\\.kt$"), null, TargetBackend.JVM_IR, true);
                }

                @Test
                @TestMetadata("kt56072.kt")
                public void testKt56072() throws Exception {
                    runTest("compiler/fir/fir2ir/testData/codegen/box/properties/synthetic/kt56072.kt");
                }
            }
        }
    }

    @Nested
    @TestMetadata("compiler/fir/fir2ir/testData/codegen/boxWithStdLib")
    @TestDataPath("$PROJECT_ROOT")
    public class BoxWithStdLib {
        @Test
        public void testAllFilesPresentInBoxWithStdLib() throws Exception {
            KtTestUtil.assertAllTestsPresentByMetadataWithExcluded(this.getClass(), new File("compiler/fir/fir2ir/testData/codegen/boxWithStdLib"), Pattern.compile("^(.+)\\.kt$"), null, TargetBackend.JVM_IR, true);
        }

        @Nested
        @TestMetadata("compiler/fir/fir2ir/testData/codegen/boxWithStdLib/enum")
        @TestDataPath("$PROJECT_ROOT")
        public class Enum {
            @Test
            public void testAllFilesPresentInEnum() throws Exception {
                KtTestUtil.assertAllTestsPresentByMetadataWithExcluded(this.getClass(), new File("compiler/fir/fir2ir/testData/codegen/boxWithStdLib/enum"), Pattern.compile("^(.+)\\.kt$"), null, TargetBackend.JVM_IR, true);
            }

            @Test
            @TestMetadata("k54079.kt")
            public void testK54079() throws Exception {
                runTest("compiler/fir/fir2ir/testData/codegen/boxWithStdLib/enum/k54079.kt");
            }
        }

        @Nested
        @TestMetadata("compiler/fir/fir2ir/testData/codegen/boxWithStdLib/properties")
        @TestDataPath("$PROJECT_ROOT")
        public class Properties {
            @Test
            public void testAllFilesPresentInProperties() throws Exception {
                KtTestUtil.assertAllTestsPresentByMetadataWithExcluded(this.getClass(), new File("compiler/fir/fir2ir/testData/codegen/boxWithStdLib/properties"), Pattern.compile("^(.+)\\.kt$"), null, TargetBackend.JVM_IR, true);
            }

            @Nested
            @TestMetadata("compiler/fir/fir2ir/testData/codegen/boxWithStdLib/properties/backingField")
            @TestDataPath("$PROJECT_ROOT")
            public class BackingField {
                @Test
                public void testAllFilesPresentInBackingField() throws Exception {
                    KtTestUtil.assertAllTestsPresentByMetadataWithExcluded(this.getClass(), new File("compiler/fir/fir2ir/testData/codegen/boxWithStdLib/properties/backingField"), Pattern.compile("^(.+)\\.kt$"), null, TargetBackend.JVM_IR, true);
                }

                @Test
                @TestMetadata("backingFieldWithSmartTypeParameters.kt")
                public void testBackingFieldWithSmartTypeParameters() throws Exception {
                    runTest("compiler/fir/fir2ir/testData/codegen/boxWithStdLib/properties/backingField/backingFieldWithSmartTypeParameters.kt");
                }
            }
        }
    }
}
