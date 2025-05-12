// 将页面所有内容封装在this.$site 参数key为contentStripped

module.exports = (options, ctx) => ({
  name: 'content-extractor',
  extendPageData(page) {
    const rawContent = page._content || page._contentStripped || ''
    page.contentStripped = rawContent.replace(/<\/?[^>]+(>|$)/g, '')
  },
})



