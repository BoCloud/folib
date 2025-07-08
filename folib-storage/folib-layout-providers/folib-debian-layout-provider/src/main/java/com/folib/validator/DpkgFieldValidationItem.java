package com.folib.validator;

import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 * @since 2024-09-03 13:52
 */
@Data
@NoArgsConstructor
public class DpkgFieldValidationItem {

    private String dpkgKey;

    private String dpkgValue;

    public DpkgFieldValidationItem(String dpkgKey, String dpkgValue) {
        this.dpkgKey = dpkgKey;
        this.dpkgValue = dpkgValue;
    }
}
