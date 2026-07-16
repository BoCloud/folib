// 获取全部locales目录
const localeFiles = require.context('../../src', true, /locale\/index\.js$/)
// 组织locales为数组
const applocales = localeFiles.keys().reduce((locales, localePath) => {
    const value = localeFiles(localePath)
    const path = localePath.replace('/locale', '').match(/\/(\S*)\//)[1]
    locales.push({
        path: path.split('/').slice(-1)[0],
        value: value.default,
    })
    return locales
}, [])
function transformStr3(str) {
    const re = /-(\w)/g
    return str.replace(re, ($0, $1) => $1.toUpperCase())
}
const locales = applocales.reduce((prev, cur) => {
    const { en, zh } = prev
    const camelPath = transformStr3(cur.path)
    en[camelPath] = cur.value.en
    zh[camelPath] = cur.value.zh
    return prev
}, {
    en: {},
    zh: {}
})
export default locales
