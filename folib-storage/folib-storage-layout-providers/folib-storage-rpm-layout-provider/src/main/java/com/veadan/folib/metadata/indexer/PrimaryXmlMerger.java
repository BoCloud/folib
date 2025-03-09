package com.veadan.folib.metadata.indexer;

import com.veadan.folib.metadata.indexer.xml.JsonToXmlConverter;
import com.veadan.folib.metadata.indexer.xml.Package;
import com.veadan.folib.metadata.indexer.xml.XmlParser;
import com.veadan.folib.providers.io.RepositoryPath;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;


public class PrimaryXmlMerger {

    private static final Logger logger = LoggerFactory.getLogger(PrimaryXmlMerger.class);

    public void mergePrimaryXmlFiles(List<RepositoryPath> xmlFilePaths, String savePath) throws Exception {
        XmlParser parser = new XmlParser();
        Set<Package> packageSet = new HashSet<>(); // 使用 HashSet 去重
        try {
            for (RepositoryPath filePath : xmlFilePaths) {
                List<Package> packages = parser.parse(filePath).getPackageList();
                packageSet.addAll(packages); // 添加到 HashSet 中自动去重
            }

            List<Package> packages = new ArrayList<>(packageSet); // 转换回 List

            // 调用 JsonToXmlConverter 转换为 XML 文件
            JsonToXmlConverter converter = new JsonToXmlConverter();
            converter.jsonToXml(packages, savePath + "/primary.xml");
            logger.info("Successfully merged primary XML files and saved to: {}", savePath + "/primary.xml");
        } catch (NoClassDefFoundError e) {
            logger.error("Failed to load JsonToXmlConverter class. Ensure the class is in the runtime classpath.", e);
            e.printStackTrace();
            throw e;

        } catch (Exception e) {
            e.printStackTrace();
            logger.error("An error occurred while merging primary XML files: {}", e.getMessage(), e);
            throw e;
        }
    }

}
