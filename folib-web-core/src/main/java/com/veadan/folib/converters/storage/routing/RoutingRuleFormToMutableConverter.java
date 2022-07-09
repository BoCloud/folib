package com.veadan.folib.converters.storage.routing;

import com.veadan.folib.forms.storage.routing.RoutingRuleForm;
import com.veadan.folib.storage.routing.MutableRoutingRule;
import com.veadan.folib.storage.routing.MutableRoutingRuleRepository;

import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.springframework.core.convert.converter.Converter;

/**
 * @author Veadan
 * @author veadan
 */
public class RoutingRuleFormToMutableConverter
        implements Converter<RoutingRuleForm, MutableRoutingRule>
{

    @Override
    public MutableRoutingRule convert(RoutingRuleForm routingRuleForm)
    {
        MutableRoutingRule rule = new MutableRoutingRule();
        rule.setGroupRepositoryId(StringUtils.trimToEmpty(routingRuleForm.getGroupRepositoryId()));
        rule.setStorageId(StringUtils.trimToEmpty(routingRuleForm.getStorageId()));
        rule.setType(routingRuleForm.getType().getType());
        rule.setPattern(routingRuleForm.getPattern());
        rule.setRepositories(
                routingRuleForm.getRepositories()
                               .stream()
                               .map(r -> {
                                   MutableRoutingRuleRepository repository = new MutableRoutingRuleRepository();
                                   repository.setRepositoryId(StringUtils.trimToEmpty(r.getRepositoryId()));
                                   repository.setStorageId(StringUtils.trimToEmpty(r.getStorageId()));
                                   return repository;
                               })
                               .collect(Collectors.toList())
        );

        return rule;
    }
}
