import Vue from 'vue'
import VueI18n from 'vue-i18n' // 引入 i18n 模块
import store from "@/store";

Vue.use(VueI18n)

// 创建国际化实例
import messages from '@/utils/loadLocale'

const i18n = new VueI18n({
    locale: sessionStorage.getItem('language') || 'zh',
    fallbackLocale: sessionStorage.getItem('language') || 'zh',
    messages,
})

// 切换语言
export const setLanguage = (lang) => {
    if (lang === undefined) {
        lang = window.sessionStorage.getItem('language') ? window.sessionStorage.getItem('language') : 'zh';
    }
    window.sessionStorage.setItem('language', lang)
    store.commit('changeLanguage', lang)
    Vue.config.lang = lang
    i18n.locale = lang
}
const lang = sessionStorage.getItem('language') || 'zh'
setLanguage(lang)

window.$t = i18n
export default i18n
