package com.folib.validator;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang.StringUtils;

import java.util.regex.Pattern;

/**
 * @author veadan
 * @since 2024-09-03 13:55
 */
@Slf4j
public class DpkgDependenciesValidator  implements DpkgPackageMetadataValidator{
    private static final Pattern PACKAGE_NAME_VALIDATION_REGEX = Pattern.compile("[a-z0-9+-.]+");
    private boolean emptyFieldValidation;

    public DpkgDependenciesValidator(boolean emptyFieldValidation) {
        this.emptyFieldValidation = emptyFieldValidation;
    }

    public void validate(String key, String... values) throws MetadataValidationException {
        log.debug("Starting to validate metadata");
        if (StringUtils.isBlank(key)) {
            log.debug("Depends line is empty. quitting the validator");
        } else if (!this.emptyFieldValidation || values != null && values.length != 0) {
            if (values != null) {
                for (String dependency : values) {
                    String dependencyAfterParsing = dependency.trim().replaceAll(" +", " ");
                    if (dependencyAfterParsing.isEmpty()) {
                        throw new MetadataValidationException("'Depends' field missing package name where package name expected");
                    }
                    for(int i = 0; i < dependencyAfterParsing.length(); ++i) {
                        char c = dependencyAfterParsing.charAt(i);
                        if (c == ' ') {
                            String charBefore = dependencyAfterParsing.substring(i - 1, i);
                            String charAfter = dependencyAfterParsing.substring(i + 1, i + 2);
                            if (PACKAGE_NAME_VALIDATION_REGEX.matcher(charBefore).matches() && PACKAGE_NAME_VALIDATION_REGEX.matcher(charAfter).matches()) {
                                throw new MetadataValidationException("'Depends' field missing package name, or invalid string where package name expected");
                            }
                        }
                    }
                }
            }
        } else {
            throw new MetadataValidationException("'Depends' field exists but has an empty value");
        }
    }
}
