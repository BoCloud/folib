package com.veadan.folib.security.vote;

import java.util.Arrays;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.AccessDecisionManager;
import org.springframework.security.access.vote.AffirmativeBased;
import org.springframework.security.access.vote.AuthenticatedVoter;
import org.springframework.security.access.vote.RoleVoter;
import org.springframework.stereotype.Component;

/**
 * @author Alex Oreshkevich
 */
@Component
public class MethodAccessDecisionManager
        extends AffirmativeBased
        implements AccessDecisionManager
{

    @SuppressWarnings("unchecked")
    public MethodAccessDecisionManager(@Autowired ExtendedAuthoritiesVoter extendedAuthoritiesVoter)
    {
        super(Arrays.asList(extendedAuthoritiesVoter,
                            new RoleVoter(),
                            new AuthenticatedVoter()));
    }
}
