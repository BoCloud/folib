package com.veadan.folib.cron.jobs;

import com.veadan.folib.cron.domain.CronTaskConfigurationDto;
import com.veadan.folib.cron.jobs.fields.CronJobField;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.nio.file.Paths;
import java.util.Set;

import com.google.common.collect.ImmutableSet;
import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;
import groovy.lang.GroovyClassLoader;
import groovy.lang.GroovyCodeSource;

/**
 * @author Veadan
 * @author Yougeshwar
 */
public class GroovyCronJob
        extends AbstractCronJob
{

    private static final String PATH_PROPERTY = "script.path";

    // We do not require PATH_PROPERTY in the cron job definition.
    // The path is automatically set when the script is uploaded.
    private static final Set<CronJobField> FIELDS = ImmutableSet.of();

    @Override
    @SuppressFBWarnings(value = "DP_CREATE_CLASSLOADER_INSIDE_DO_PRIVILEGED")
    public void executeTask(CronTaskConfigurationDto config)
    {
        try
        {
            Class scriptClass = new GroovyClassLoader().parseClass(
                    new GroovyCodeSource(Paths.get(getScriptPath(config)).toUri()));
            Object scriptInstance = scriptClass.getConstructor().newInstance();
            //noinspection unchecked
            scriptClass.getDeclaredMethod("execute", new Class[]{}).invoke(scriptInstance);
        }
        catch (IOException | IllegalAccessException | InstantiationException | NoSuchMethodException | InvocationTargetException e)
        {
            logger.error("IOException: ", e);
        }
    }

    @Override
    public CronJobDefinition getCronJobDefinition()
    {
        return CronJobDefinition.newBuilder()
                                .jobClass(GroovyCronJob.class.getName())
                                .name("Groovy Cron Job").scope("SYSTEM")
                                .fields(FIELDS)
                                .build();
    }

    public String getScriptPath(CronTaskConfigurationDto configuration)
    {
        return configuration.getRequiredProperty(PATH_PROPERTY);
    }

}
