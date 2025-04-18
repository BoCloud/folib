package com.veadan.folib.services;

import org.antlr.analysis.MachineProbe;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * @author LingengMa
 * @date 2025/04/18 15:58
 * @Description:
 */

@Service
public class CondaCacheService {
    private final Map<String, Date> cache = new ConcurrentHashMap<>();

    public void put(String key, Date value) {
        cache.put(key, value);
    }

    public boolean containsKey(String key) {
        return cache.containsKey(key);
    }

    public Date get(String key) {
        return cache.get(key);
    }
}
