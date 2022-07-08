package com.veadan.folib.storage.routing;

import javax.annotation.concurrent.Immutable;
import java.util.Collections;
import java.util.List;

import com.google.common.collect.ImmutableList;
import static java.util.stream.Collectors.toList;

/**
 * @author veadan
 */
@Immutable
public class RoutingRules
{

    private final List<RoutingRule> rules;

    private List<RoutingRule> denied;

    private List<RoutingRule> accepted;

    public RoutingRules(final MutableRoutingRules delegate)
    {
        this.rules = immuteRoutingRules(delegate.getRules());
    }

    private List<RoutingRule> immuteRoutingRules(final List<MutableRoutingRule> source)
    {
        return source != null ? ImmutableList.copyOf(source.stream().map(RoutingRule::new).collect(toList())) :
               Collections.emptyList();
    }

    public List<RoutingRule> getRules()
    {
        return rules;
    }

    public List<RoutingRule> getDenied()
    {
        if (denied != null)
        {
            return denied;
        }
        return denied = rules.stream().filter(RoutingRule::isDeny).collect(toList());
    }

    public List<RoutingRule> getAccepted()
    {
        if (accepted != null)
        {
            return accepted;
        }
        return accepted = rules.stream().filter(RoutingRule::isAccept).collect(toList());
    }
}
