package com.folib.providers.repository.group;

import com.google.common.collect.Lists;
import com.folib.configuration.ConfigurationManager;
import com.folib.configuration.ConfigurationUtils;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;

import jakarta.inject.Inject;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Slf4j
@Component
public class GroupRepositorySetCollector
{

    @Inject
    private ConfigurationManager configurationManager;

    public Set<Repository> collect(Repository groupRepository)
    {
        return collect(groupRepository, false);
    }

    public Set<Repository> collect(Repository groupRepository,
                                   boolean traverse)
    {
        log.info("GroupRepository {}", groupRepository.getId());
        List<String> storageAndRepositoryIdList = Lists.newArrayList();
        configurationManager.resolveGroupRepository(groupRepository, storageAndRepositoryIdList);
        Set<Repository> result = storageAndRepositoryIdList.stream()
                                                .map(groupRepoId -> getRepository(groupRepository.getStorage(),
                                                                                  groupRepoId))
                                                .collect(Collectors.toCollection(LinkedHashSet::new));

        if (!traverse)
        {
            return result;
        }

        Set<Repository> traverseResult = new LinkedHashSet<>();
        for (Iterator<Repository> i = result.iterator(); i.hasNext(); )
        {
            Repository r = i.next();
            if (CollectionUtils.isEmpty(r.getGroupRepositories()))
            {
                traverseResult.add(r);
                continue;
            }

            i.remove();
            traverseResult.addAll(collect(r, true));
        }

        return traverseResult;
    }

    private Repository getRepository(Storage storage,
                                     String id)
    {
        String sId = ConfigurationUtils.getStorageId(storage.getId(), id);
        String rId = ConfigurationUtils.getRepositoryId(id);

        return configurationManager.getConfiguration().getStorage(sId).getRepository(rId);
    }

}
