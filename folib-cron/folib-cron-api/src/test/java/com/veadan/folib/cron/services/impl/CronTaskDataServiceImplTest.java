package com.veadan.folib.cron.services.impl;

import com.veadan.folib.cron.jobs.TestJavaCronJob;
import com.veadan.folib.cron.CronApiTestConfig;
import com.veadan.folib.cron.domain.CronTaskConfigurationDto;
import com.veadan.folib.cron.services.CronTaskDataService;

import javax.inject.Inject;
import java.io.IOException;
import java.util.Collection;
import java.util.UUID;
import java.util.stream.Collectors;

import com.google.common.collect.ImmutableMap;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;
import static org.assertj.core.api.Assertions.assertThat;

/**
 * @author Przemyslaw Fusik
 */
@SpringBootTest
@ActiveProfiles(profiles = "test")
@CronApiTestConfig
class CronTaskDataServiceImplTest
{

    private static final String PROPERTY_REPOSITORY_ID = "repositoryId";

    private static final String PROPERTY_STORAGE_ID = "storageId";

    private static final String REPOSITORY_RELEASES = "releases";

    private static final String STORAGE_0 = "storage0";


    @Inject
    private CronTaskDataService cronTaskDataService;

    @Test
    void shouldForbidDuplications()
            throws IOException
    {
        assertSingleInitialInstance();

        CronTaskConfigurationDto newDto = new CronTaskConfigurationDto();
        newDto.setJobClass(TestJavaCronJob.class.getName());
        newDto.setProperties(
                ImmutableMap.of(PROPERTY_REPOSITORY_ID, REPOSITORY_RELEASES, PROPERTY_STORAGE_ID, STORAGE_0));

        cronTaskDataService.save(newDto);

        assertSingleInitialInstance();
    }

    private void assertSingleInitialInstance()
    {
        Collection<CronTaskConfigurationDto> dtos = findByClass(TestJavaCronJob.class);
        assertThat(dtos).hasSize(1);

        CronTaskConfigurationDto dto = dtos.iterator().next();
        assertThat(dto.getUuid()).isEqualTo(UUID.fromString("baa164a5-c900-43ff-8d13-7a0d0e509d1d"));
        assertThat(dto.getRequiredProperty(PROPERTY_REPOSITORY_ID)).isEqualTo(REPOSITORY_RELEASES);
        assertThat(dto.getRequiredProperty(PROPERTY_STORAGE_ID)).isEqualTo(STORAGE_0);
        assertThat(dto.getJobClass()).isEqualTo(TestJavaCronJob.class.getName());
    }

    private Collection<CronTaskConfigurationDto> findByClass(Class<?> expectedClass)
    {
        return cronTaskDataService.getTasksConfigurationDto()
                                  .getCronTaskConfigurations()
                                  .stream()
                                  .filter(dto -> dto.getJobClass().equals(expectedClass.getName()))
                                  .collect(Collectors.toList());
    }


}
