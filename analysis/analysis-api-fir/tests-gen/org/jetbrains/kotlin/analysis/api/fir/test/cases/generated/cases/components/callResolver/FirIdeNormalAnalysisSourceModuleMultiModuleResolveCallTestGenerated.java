/*
 * Copyright 2010-2023 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.analysis.api.fir.test.cases.generated.cases.components.callResolver;

import com.intellij.testFramework.TestDataPath;
import org.jetbrains.kotlin.test.util.KtTestUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.kotlin.analysis.api.fir.test.configurators.AnalysisApiFirTestConfiguratorFactory;
import org.jetbrains.kotlin.analysis.test.framework.test.configurators.AnalysisApiTestConfiguratorFactoryData;
import org.jetbrains.kotlin.analysis.test.framework.test.configurators.AnalysisApiTestConfigurator;
import org.jetbrains.kotlin.analysis.test.framework.test.configurators.TestModuleKind;
import org.jetbrains.kotlin.analysis.test.framework.test.configurators.FrontendKind;
import org.jetbrains.kotlin.analysis.test.framework.test.configurators.AnalysisSessionMode;
import org.jetbrains.kotlin.analysis.test.framework.test.configurators.AnalysisApiMode;
import org.jetbrains.kotlin.analysis.api.impl.base.test.cases.components.callResolver.AbstractMultiModuleResolveCallTest;
import org.jetbrains.kotlin.test.TestMetadata;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.util.regex.Pattern;

/** This class is generated by {@link org.jetbrains.kotlin.generators.tests.analysis.api.GenerateAnalysisApiTestsKt}. DO NOT MODIFY MANUALLY */
@SuppressWarnings("all")
@TestMetadata("analysis/analysis-api/testData/components/multiModuleCallResolver/resolveCall")
@TestDataPath("$PROJECT_ROOT")
public class FirIdeNormalAnalysisSourceModuleMultiModuleResolveCallTestGenerated extends AbstractMultiModuleResolveCallTest {
    @NotNull
    @Override
    public AnalysisApiTestConfigurator getConfigurator() {
        return AnalysisApiFirTestConfiguratorFactory.INSTANCE.createConfigurator(
            new AnalysisApiTestConfiguratorFactoryData(
                FrontendKind.Fir,
                TestModuleKind.Source,
                AnalysisSessionMode.Normal,
                AnalysisApiMode.Ide
            )
        );
    }

    @Test
    public void testAllFilesPresentInResolveCall() throws Exception {
        KtTestUtil.assertAllTestsPresentByMetadataWithExcluded(this.getClass(), new File("analysis/analysis-api/testData/components/multiModuleCallResolver/resolveCall"), Pattern.compile("^(.+)\\.kt$"), null, true);
    }

    @Test
    @TestMetadata("implicitTypeSubstituteOverrideFromOtherModule.kt")
    public void testImplicitTypeSubstituteOverrideFromOtherModule() throws Exception {
        runTest("analysis/analysis-api/testData/components/multiModuleCallResolver/resolveCall/implicitTypeSubstituteOverrideFromOtherModule.kt");
    }

    @Test
    @TestMetadata("unitTypeFromOtherModule.kt")
    public void testUnitTypeFromOtherModule() throws Exception {
        runTest("analysis/analysis-api/testData/components/multiModuleCallResolver/resolveCall/unitTypeFromOtherModule.kt");
    }

    @Nested
    @TestMetadata("analysis/analysis-api/testData/components/multiModuleCallResolver/resolveCall/withTestCompilerPluginEnabled")
    @TestDataPath("$PROJECT_ROOT")
    public class WithTestCompilerPluginEnabled {
        @Test
        public void testAllFilesPresentInWithTestCompilerPluginEnabled() throws Exception {
            KtTestUtil.assertAllTestsPresentByMetadataWithExcluded(this.getClass(), new File("analysis/analysis-api/testData/components/multiModuleCallResolver/resolveCall/withTestCompilerPluginEnabled"), Pattern.compile("^(.+)\\.kt$"), null, true);
        }

        @Test
        @TestMetadata("annotationFromOtherModule.kt")
        public void testAnnotationFromOtherModule() throws Exception {
            runTest("analysis/analysis-api/testData/components/multiModuleCallResolver/resolveCall/withTestCompilerPluginEnabled/annotationFromOtherModule.kt");
        }
    }
}
