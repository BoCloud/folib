package com.veadan.folib.services.support;

import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.routing.RepositoryIdentifiable;
import com.veadan.folib.storage.routing.RoutingRule;
import com.veadan.folib.storage.routing.RoutingRules;

import javax.inject.Inject;
import java.io.IOException;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 * @see <a href="https://folib.github.io/user-guide/artifact-routing-rules.html">Artifact Routing Rules</a>
 */
@Component
public class ArtifactRoutingRulesChecker
{

    @Inject
    private ConfigurationManager configurationManager;

    public boolean isDenied(Repository groupRepository,
                            RepositoryPath repositoryPath)
            throws IOException
    {
        final RoutingRules routingRules = configurationManager.getConfiguration().getRoutingRules();
        final boolean hasDenyRules = hasCandidates(groupRepository, repositoryPath, routingRules.getDenied());
        final boolean hasAcceptRules = hasCandidates(groupRepository, repositoryPath, routingRules.getAccepted());

        return hasDenyRules && !hasAcceptRules;
    }

    private boolean hasCandidates(Repository groupRepository,
                                  RepositoryPath repositoryPath,
                                  List<RoutingRule> routingRules)
            throws IOException
    {

        String artifactPath = RepositoryFiles.relativizePath(repositoryPath);
        Repository subRepository = repositoryPath.getRepository();

        return routingRules.stream()
                           .anyMatch(rule -> {
                               boolean result = false;

                               if ((isMatch(rule, groupRepository))
                                   && rule.getRegex().matcher(artifactPath).matches())
                               {
                                   // an empty collection means the rule is applied to **all** repositories in the group.
                                   if (rule.getRepositories().size() == 0)
                                   {
                                       result = true;
                                   }
                                   else
                                   {
                                       result = rule.getRepositories()
                                                    .stream()
                                                    .anyMatch(r -> isMatch(r, subRepository));
                                   }
                               }

                               return result;
                           });

    }

    private boolean isMatch(RepositoryIdentifiable rule,
                            Repository repository)
    {
        boolean result = false;

        // exact match == storageId:repositoryId
        if (equalsIgnoreCase(rule.getStorageIdAndRepositoryId(), repository.getStorageIdAndRepositoryId()))
        {
            result = true;
        }
        // wildcard == *:*
        else if (equalsIgnoreCase(rule.getStorageIdAndRepositoryId(), StringUtils.EMPTY))
        {
            result = true;
        }
        // wildcard == storageId:*
        else if (equalsIgnoreCase(rule.getRepositoryId(), StringUtils.EMPTY) &&
                 equalsIgnoreCase(rule.getStorageId(), repository.getStorage().getId()))
        {
            result = true;
        }
        // wildcard == *:repositoryId
        else if (equalsIgnoreCase(rule.getStorageId(), StringUtils.EMPTY) &&
                 equalsIgnoreCase(rule.getRepositoryId(), repository.getId()))
        {
            result = true;
        }

        return result;
    }

    private boolean equalsIgnoreCase(final String a,
                                     final String b)
    {
        return StringUtils.trimToEmpty(a).equalsIgnoreCase(StringUtils.trimToEmpty(b));
    }
}
