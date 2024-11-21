package com.veadan.folib.domain.migrate;

import com.alibaba.excel.context.AnalysisContext;
import com.alibaba.excel.event.AnalysisEventListener;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang.StringUtils;

import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author huayanjun
 * @since 2024-11-20 10:27
 */
@Slf4j
public class BatchChangeListener extends AnalysisEventListener<BatchChangeRepository> {

    private final List<BatchChangeRepository> repositories = new LinkedList<>();

    @Override
    public void invoke(BatchChangeRepository batchChangeRepository, AnalysisContext analysisContext) {
        repositories.add(batchChangeRepository);
    }

    @Override
    public void doAfterAllAnalysed(AnalysisContext analysisContext) {
        log.info("all data parsed");

    }

    public List<BatchChangeRepository> getRepositories() {
        return this.repositories;
    }

    public List<BatchChangeRepository> getDoneRepositories() {
        return this.repositories.stream().filter(e->"已完成".equals(StringUtils.trim(e.getStatus()))).collect(Collectors.toList());
    }

}
