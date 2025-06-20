package com.veadan.folib.validator;

/**
 * @author huayanjun
 * @since 2024-09-03 13:43
 */
public interface DpkgPackageMetadataValidator {
    void validate(String key, String... values) throws MetadataValidationException;
}
