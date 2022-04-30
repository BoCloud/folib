package com.veadan.folib.testing;

import javax.annotation.PostConstruct;
import java.util.LinkedHashMap;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

/**
 * @author mtodorov
 */
@Component
public class AssignedPorts
{

    private static final Logger logger = LoggerFactory.getLogger(AssignedPorts.class);

    /**
     * K: port key
     * V: the port
     */
    private Map<String, Integer> ports = new LinkedHashMap<>();


    public AssignedPorts()
    {
    }

    @PostConstruct
    public void initializePorts()
    {
        for (Object key : System.getProperties().keySet())
        {
            String portKey = key.toString();
            if (portKey.startsWith("port.") && !StringUtils.isEmpty(System.getProperty(portKey).isEmpty()))
            {
                final int port = Integer.parseInt(System.getProperty(portKey));
                ports.put(portKey, port);

                logger.debug("Discovered port '{}' = '{}'.", portKey, port);
            }
        }
    }

    public int getPort(String portKey)
    {
        return ports.get(portKey);
    }

    public Map<String, Integer> getPorts()
    {
        return ports;
    }

    public void setPorts(Map<String, Integer> ports)
    {
        this.ports = ports;
    }

}
