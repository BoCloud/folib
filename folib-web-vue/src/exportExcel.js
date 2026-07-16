import ExcelJS from 'exceljs';
import { saveAs } from 'file-saver';

export default async function exportExcel(data, headers, fileName = '制品扫描.xlsx') {
    const workbook = new ExcelJS.Workbook();
    const worksheet = workbook.addWorksheet('Sheet1');

    worksheet.columns = headers;

    // 设置表头样式
    worksheet.getRow(1).font = { bold: true, color: { argb: 'FFFFFF' } };
    worksheet.getRow(1).alignment = { vertical: 'middle', horizontal: 'center' };
    worksheet.getRow(1).fill = {
        type: 'pattern',
        pattern: 'solid',
        fgColor: { argb: '0070C0' },
    };
    worksheet.getRow(1).border = {
        top: { style: 'thin' },
        left: { style: 'thin' },
        bottom: { style: 'thin' },
        right: { style: 'thin' },
    };

    // 添加数据
    data.forEach((item, index) => {
        const { storageId, repositoryId, artifactPath, lastUsed, downloadCount, sizeInBytes } = item
        const params = {
            storageId,
            repositoryId,
            artifactPath,
            lastUsed,
            downloadCount,
            sizeInBytes
        }
        const row = worksheet.addRow(params);

        // 设置富文本样式的列（第 7 列，即 G 列）
        const richTextCell = worksheet.getCell(`G${index + 2}`); // G 列（第 7 列）
        richTextCell.value = {
            richText: item.vulnerabilitiesCount.map((part) => ({
                text: `${part.key}：${part.value}，`,
                font: {
                    color: { argb: part.color },
                    bold: part.bold || false,
                    italic: part.italic || false,
                    size: part.size || 12,
                    name: part.font || 'Arial',
                },
            })),
        };

        row.alignment = { vertical: 'middle', horizontal: 'left' };
        row.font = { color: { argb: '000000' }, size: 12 };

        // 设置奇偶行背景色
        row.fill = index % 2 === 0
            ? { type: 'pattern', pattern: 'solid', fgColor: { argb: 'F5F5F5' } }
            : { type: 'pattern', pattern: 'solid', fgColor: { argb: 'FFFFFF' } };
    });

    // 保存文件
    const buffer = await workbook.xlsx.writeBuffer();
    saveAs(new Blob([buffer], { type: 'application/octet-stream' }), fileName);
}