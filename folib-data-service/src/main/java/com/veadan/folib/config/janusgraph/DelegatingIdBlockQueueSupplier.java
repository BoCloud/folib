package com.veadan.folib.config.janusgraph;

import java.util.Optional;
import java.util.function.Supplier;

import org.springframework.stereotype.Component;

@Component
public class DelegatingIdBlockQueueSupplier implements Supplier<String>
{

    private Supplier<String> target;

    public Supplier<String> getTarget()
    {
        return target;
    }

    public void setTarget(Supplier<String> target)
    {
        this.target = target;
    }

    @Override
    public String get()
    {
        return Optional.ofNullable(target).map(Supplier::get).orElse(null);
    }

}
