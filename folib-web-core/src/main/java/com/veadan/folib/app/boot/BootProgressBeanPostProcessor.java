package com.veadan.folib.app.boot;

import com.veadan.folib.config.SwaggerConfig;
import com.veadan.folib.config.WebSecurityConfig;
import com.veadan.folib.config.hazelcast.HazelcastConfiguration;
import com.veadan.folib.web.DirectoryTraversalFilter;

import javax.annotation.Nullable;
import javax.servlet.ServletRegistration;
import java.util.Map;
import java.util.concurrent.locks.LockSupport;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import io.reactivex.Observable;
import io.reactivex.schedulers.Schedulers;
import io.reactivex.subjects.BehaviorSubject;
import io.reactivex.subjects.Subject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.boot.web.embedded.jetty.JettyServletWebServerFactory;
import org.springframework.boot.web.server.WebServer;
import org.springframework.context.SmartLifecycle;
import org.springframework.core.annotation.Order;
import org.springframework.security.authentication.DefaultAuthenticationEventPublisher;
import org.springframework.stereotype.Component;
import static org.springframework.core.Ordered.HIGHEST_PRECEDENCE;

/**
 * <p>
 * This bean is responsible for bringing up a temporary web server which allows clients to connect and only view the
 * static assets as well as receive preliminary status report via the /api/ping endpoint (via EventSource).
 * </p>
 * <p>
 * Requirements:
 *  <li>Must start as soon as possible.</li>
 *  <li>Must stop BEFORE {@link org.springframework.boot.web.servlet.context.WebServerStartStopLifecycle} which will start the real web server.</li>
 *  <li>Must emit boot progress via /api/ping</li>
 * </p>
 */
@Component
@Order(HIGHEST_PRECEDENCE)
public class BootProgressBeanPostProcessor
        implements BeanPostProcessor, SmartLifecycle
{

    private static final Logger logger = LoggerFactory.getLogger(BootProgressBeanPostProcessor.class);

    private final Subject<String> progress = BehaviorSubject.create();
    private WebServer webServer;
    private boolean beanIsRunning;

    //@formatter:off
    private static final Map<String, String> displayMessages = Stream.of(new String[][]{
            { "management.metrics-org.springframework.boot.actuate.autoconfigure.metrics.MetricsProperties", "Loading metrics..." },
            { "orientDbServer", "Waiting for database..." },
            { "liquibase", "Running update scripts..." },
            { HazelcastConfiguration.class.getName(), "Waiting for cache..." },
            { "org.springframework.boot.actuate.autoconfigure", "Waiting for actuators..." },
            // it might look like we need to use "storageBooter" here, but that actually depends on tempDirBooter which does most of the heavy lifting.
            { "tempDirBooter", "Waiting for storage booter" },
            { "storageProviderRegistry", "Loading storage providers..." },
            { "maven2LayoutProvider", "Loading Maven layout provider.." },
            { "nugetLayoutProvider", "Loading Nuget layout provider.." },
            { "npmLayoutProvider", "Loading NPM layout provider.." },
            { "rpmLayoutProvider", "Loading Rpm layout provider.." },
            { "rawLayoutProvider", "Loading Raw layout provider.." },
            { "transactionManager", "Loading transaction manager..." },
            { WebSecurityConfig.class.getName(), "Loading security configuration..." },
            { SwaggerConfig.class.getName(), "Loading documentation..." },
            { "fallback", "Waiting for services to go live..."},
    }).collect(Collectors.toMap(p -> p[0], p -> p[1]));
    //@formatter:on

    /**
     * Counter-intuitive, but since we need the web server to stop exactly before {@link org.springframework.boot.web.servlet.context.WebServerStartStopLifecycle}
     * we are forced to use {@link SmartLifecycle#start}.
     */
    @Override
    public void start()
    {
        stopWebServer();
        beanIsRunning = true;
    }

    @Override
    public void stop()
    {
        beanIsRunning = false;
    }

    @Override
    public boolean isRunning()
    {
        return beanIsRunning;
    }

    /**
     * The phase should be lower then {@link org.springframework.boot.web.servlet.context.WebServerStartStopLifecycle#getPhase()}
     */
    @Override
    public int getPhase()
    {
        return Integer.MAX_VALUE - 2;
    }

    /**
     * We are hooking into the postProcessBeforeInitialization in order to track the boot progress and emit messages
     * to connected clients.
     */
    @Override
    public Object postProcessBeforeInitialization(Object bean,
                                                  String beanName)
            throws BeansException
    {
        logger.trace("Found bean: {}", beanName);

        if (displayMessages.containsKey(beanName) ||
            beanName.contains(DefaultAuthenticationEventPublisher.class.getName()))
        {
            String displayMessage = displayMessages.getOrDefault(beanName, displayMessages.get("fallback"));
            progress.onNext(displayMessage);
        }

        return bean;
    }

    /**
     * <p>Might look unnecessary, but this allows us to get an {@link JettyServletWebServerFactory} instance
     * after it has been properly populated/configured with all settings (i.e. correct port) and then
     * use it as a base to start the temporary server.</p>
     * <p>Removing this will likely result in a default {@link JettyServletWebServerFactory} instance working at port 8080</p>
     */
    @Override
    @Nullable
    public Object postProcessAfterInitialization(Object bean,
                                                 String beanName)
            throws BeansException
    {
        if (bean instanceof JettyServletWebServerFactory)
        {
            startWebServer((JettyServletWebServerFactory) bean);
        }
        
        return bean;
    }

    private Observable<String> getProgressObservable()
    {
        return progress.observeOn(Schedulers.single()).subscribeOn(Schedulers.io()).onTerminateDetach();
    }

    private boolean isWebServerRunning()
    {
        return webServer != null;
    }

    private void startWebServer(JettyServletWebServerFactory factory)
    {
        if (isWebServerRunning())
        {
            return;
        }
        
        logger.info("Eagerly starting temporary web server {}", factory.toString());
        webServer = factory.getWebServer(context -> {
            ServletRegistration.Dynamic defaultServlet = context.addServlet("default", new BootProgressServlet(getProgressObservable()));
            defaultServlet.addMapping("/", BootProgressServlet.pingRequestURI);
            defaultServlet.setLoadOnStartup(1);
            defaultServlet.setAsyncSupported(true);

            context.addFilter(DirectoryTraversalFilter.class.getName(), DirectoryTraversalFilter.class);
        });

        webServer.start();
    }

    private void stopWebServer()
    {
        if (!isWebServerRunning())
        {
            return;
        }
        
        logger.info("Notifying clients Folib has booted.");
        progress.onComplete();

        // Slightly delay stopping the server to leave enough time for existing responses to complete.
        // Fixes
        //   curl: (18) transfer closed with outstanding read data remaining
        //   err_incomplete_chunked_encoding
        // NB: This needs to be in sync with the UI.
        LockSupport.parkUntil(System.currentTimeMillis() + 1000);

        logger.info("Stopping temporary server {}", webServer.toString());
        webServer.stop();
        webServer = null;
    }

}
