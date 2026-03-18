package consulo.typescript.language;

import consulo.localize.LocalizeValue;
import consulo.typescript.localize.TypeScriptLocalize;

import java.util.function.Supplier;

/**
 * TypeScript-specific features. Each feature is registered to a version
 * via {@code addTypeScriptFeature()} in the version class constructor chain.
 *
 * @author VISTALL
 * @since 2026-03-17
 */
public enum TypeScriptFeature {
    INTERFACE(TypeScriptLocalize::featureInterface),
    ENUM(TypeScriptLocalize::featureEnum),
    GENERIC(TypeScriptLocalize::featureGeneric),
    TYPE_ANNOTATION(TypeScriptLocalize::featureTypeAnnotation),
    ACCESS_MODIFIER(TypeScriptLocalize::featureAccessModifier),
    NAMESPACE(TypeScriptLocalize::featureNamespace),
    TYPE_ALIAS(TypeScriptLocalize::featureTypeAlias),
    UNION_TYPE(TypeScriptLocalize::featureUnionType),
    INTERSECTION_TYPE(TypeScriptLocalize::featureIntersectionType),
    DECORATOR(TypeScriptLocalize::featureDecorator),
    ABSTRACT_CLASS(TypeScriptLocalize::featureAbstractClass),
    COMPUTED_PROPERTY(TypeScriptLocalize::featureComputedProperty),
    READONLY_MODIFIER(TypeScriptLocalize::featureReadonlyModifier),
    NEVER_TYPE(TypeScriptLocalize::featureNeverType),
    NON_NULL_ASSERTION(TypeScriptLocalize::featureNonNullAssertion),
    MAPPED_TYPE(TypeScriptLocalize::featureMappedType),
    KEYOF_OPERATOR(TypeScriptLocalize::featureKeyofOperator),
    INDEXED_ACCESS_TYPE(TypeScriptLocalize::featureIndexedAccessType),
    OBJECT_SPREAD(TypeScriptLocalize::featureObjectSpread),
    CONDITIONAL_TYPE(TypeScriptLocalize::featureConditionalType),
    INFER_KEYWORD(TypeScriptLocalize::featureInferKeyword),
    UNKNOWN_TYPE(TypeScriptLocalize::featureUnknownType),
    REST_TUPLE(TypeScriptLocalize::featureRestTuple),
    LABELED_TUPLE(TypeScriptLocalize::featureLabeledTuple),
    VARIADIC_TUPLE(TypeScriptLocalize::featureVariadicTuple),
    TEMPLATE_LITERAL_TYPE(TypeScriptLocalize::featureTemplateLiteralType),
    KEY_REMAPPING(TypeScriptLocalize::featureKeyRemapping),
    OVERRIDE_MODIFIER(TypeScriptLocalize::featureOverrideModifier),
    TYPE_ONLY_IMPORT_SPECIFIER(TypeScriptLocalize::featureTypeOnlyImportSpecifier),
    SATISFIES_OPERATOR(TypeScriptLocalize::featureSatisfiesOperator),
    CONST_TYPE_PARAMETER(TypeScriptLocalize::featureConstTypeParameter),
    DECORATOR_METADATA(TypeScriptLocalize::featureDecoratorMetadata),
    IMPORT_ATTRIBUTES(TypeScriptLocalize::featureImportAttributes),
    IMPORT_DEFER(TypeScriptLocalize::featureImportDefer),
    SUBPATH_IMPORTS(TypeScriptLocalize::featureSubpathImports);

    private final Supplier<LocalizeValue> myDisplayName;

    TypeScriptFeature(Supplier<LocalizeValue> displayName) {
        myDisplayName = displayName;
    }

    public LocalizeValue getDisplayName() {
        return myDisplayName.get();
    }
}
