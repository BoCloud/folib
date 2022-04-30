package com.veadan.folib.validation;

import javax.inject.Inject;
import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

import com.veadan.folib.authorization.service.AuthorizationConfigService;
import org.springframework.util.StringUtils;

/**
 * @author Pablo Tirado
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
                || !authorizationConfigService.get().getRoles().stream().anyMatch(r -> r.getName().equals(roleName));
    }

}
