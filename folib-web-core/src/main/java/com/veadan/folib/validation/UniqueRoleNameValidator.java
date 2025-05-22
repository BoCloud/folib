package com.veadan.folib.validation;

import com.veadan.folib.authorization.service.AuthorizationConfigService;
import jakarta.inject.Inject;
import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import org.springframework.util.StringUtils;

/**
 * @author Veadan
 */
public class UniqueRoleNameValidator
        implements ConstraintValidator<UniqueRoleName, String>
{

    @Inject
    private AuthorizationConfigService authorizationConfigService;

    @Override
    public void initialize(UniqueRoleName constraint)
    {
        // empty by design
    }

    @Override
    public boolean isValid(String roleName,
                           ConstraintValidatorContext context)
    {
        return StringUtils.isEmpty(roleName)
                || authorizationConfigService.get().getRoles().stream().noneMatch(r -> r.getName().equalsIgnoreCase(roleName));
    }

}
