package com.folib.util;

import com.folib.domain.PypiPackageInfo;
import org.springframework.stereotype.Component;

import javax.validation.*;
import java.util.Set;

@Component
public class PypiPackageInfoValidator
{

    public boolean validate(PypiPackageInfo packageInfo)
            throws ConstraintViolationException
    {
        ValidatorFactory factory = Validation.buildDefaultValidatorFactory();
        Validator validator = factory.getValidator();
        Set<ConstraintViolation<PypiPackageInfo>> violations = validator.validate(packageInfo);
        if (violations.size() > 0)
        {
            throw new ConstraintViolationException(violations);
        }
        return true;
    }
}
