package com.veadan.folib.providers.header;

import org.springframework.stereotype.Component;

import java.util.*;
import java.util.stream.Collectors;

/**
 * @author carlspring
 */
@Component
public class HeaderMappingRegistry
{

    public static final String USER_AGENT_UNKNOWN = "unknown/*";

    private static final String USER_AGENT_FORMAT = "%s/*";

    private Map<String, List<String>> layout2UserAgentKeywodMap = new LinkedHashMap<>();

    public HeaderMappingRegistry()
    {
    }

    public void register(String layoutProviderAlias,
                         String... userAgentKeywords)
    {
        layout2UserAgentKeywodMap.put(layoutProviderAlias,
                                      Arrays.stream(userAgentKeywords)
                                            .collect(Collectors.toList()));
    }

    public Optional<String> lookupUserAgent(String originalUserAgentHeaverValue)
    {
        if (originalUserAgentHeaverValue == null) {
            return Optional.empty();
        }
        
        return layout2UserAgentKeywodMap.values()
                                        .stream()
                                        .flatMap(l -> l.stream())
                                        .filter(s -> originalUserAgentHeaverValue.toUpperCase().contains(s.toUpperCase()))
                                        .map(this::formatHeader)
                                        .findFirst();
    }

    public String defaultLayoutUserAgent(String layout)
    {
        return Optional.ofNullable(layout2UserAgentKeywodMap.get(layout))
                       .flatMap(s -> s.stream().findFirst())
                       .map(this::formatHeader)
                       .orElse(USER_AGENT_UNKNOWN);
    }

    private String formatHeader(String s)
    {
        return String.format(USER_AGENT_FORMAT, s);
    }

}
