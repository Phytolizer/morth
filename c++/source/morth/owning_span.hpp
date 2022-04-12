#pragma once

#include "morth/shorter_types.hpp"

#include <vector>

template <typename ValueType, typename Container = std::vector<ValueType>> class OwningSpan {
    Container m_data;

  public:
    OwningSpan() = default;
    // NOLINTNEXTLINE(google-explicit-constructor)
    inline OwningSpan(Container&& data) : m_data(std::move(data)) {}
    // NOLINTNEXTLINE(google-explicit-constructor)
    inline OwningSpan(Container data) : m_data(std::move(data)) {}

    inline auto begin() const {
        return m_data.begin();
    }
    inline auto end() const {
        return m_data.end();
    }
    inline auto size() const {
        return m_data.size();
    }
    inline auto empty() const {
        return m_data.empty();
    }
    inline auto operator[](Size index) const {
        return m_data[index];
    }
};
